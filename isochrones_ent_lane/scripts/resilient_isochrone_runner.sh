#!/bin/bash
# =============================================================================
# Resilient wrapper for secondary isochrone generation
# =============================================================================
# Auto-restarts Valhalla container when it becomes unresponsive.
# Uses checkpoint/resume in the R script.
#
# Environment variables:
#   VALHALLA_HOST   Host to reach Valhalla (default: 127.0.0.1)
#   VALHALLA_PORT   Port to reach Valhalla (default: 8002)
#
# Usage: bash scripts/resilient_isochrone_runner.sh [bands]
#   bands: comma-separated drive times (default: 120,180)
#
# Rewritten: 2026-03-05 (parameterized URL, signal handling)
# =============================================================================

set -euo pipefail

BANDS="${1:-120,180}"
MAX_RETRIES=20
RETRY_DELAY=30

VALHALLA_HOST="${VALHALLA_HOST:-127.0.0.1}"
VALHALLA_PORT="${VALHALLA_PORT:-8002}"
VALHALLA_URL="http://${VALHALLA_HOST}:${VALHALLA_PORT}"
CONTAINER_NAME="${VALHALLA_CONTAINER:-valhalla_server}"

# =============================================================================
# SIGNAL HANDLING — graceful mid-batch shutdown
# =============================================================================
INTERRUPTED=0
handle_signal() {
    echo ""
    echo "[$(date)] Received shutdown signal. Finishing current batch gracefully..."
    INTERRUPTED=1
}
trap handle_signal SIGINT SIGTERM

# =============================================================================
# HEALTH CHECK
# =============================================================================
valhalla_healthy() {
    curl -sf --connect-timeout 3 --max-time 5 "${VALHALLA_URL}/status" 2>/dev/null \
        | python3 -c "import sys,json; j=json.load(sys.stdin); assert 'version' in j" 2>/dev/null
}

ensure_valhalla() {
    local attempts=0
    while ! valhalla_healthy; do
        attempts=$((attempts + 1))
        if [ $attempts -gt 5 ]; then
            echo "[$(date)] Failed to start Valhalla after 5 attempts, giving up"
            return 1
        fi
        echo "[$(date)] Valhalla not responding at ${VALHALLA_URL}, restarting container (attempt $attempts)..."
        docker restart "$CONTAINER_NAME" 2>/dev/null || true
        sleep 15
    done
    echo "[$(date)] Valhalla healthy at ${VALHALLA_URL}"
    return 0
}

# =============================================================================
# MAIN LOOP
# =============================================================================
IFS=',' read -ra BAND_ARRAY <<< "$BANDS"

for band in "${BAND_ARRAY[@]}"; do
    if [ "$INTERRUPTED" -eq 1 ]; then
        echo "[$(date)] Stopping before ${band}-minute band (interrupted)"
        break
    fi

    echo ""
    echo "============================================"
    echo "[$(date)] Starting ${band}-minute band"
    echo "============================================"

    retry=0
    while [ $retry -lt $MAX_RETRIES ]; do
        if [ "$INTERRUPTED" -eq 1 ]; then
            echo "[$(date)] Interrupted during ${band}-minute band"
            break 2
        fi

        if ! ensure_valhalla; then
            echo "[$(date)] Cannot start Valhalla, exiting"
            exit 1
        fi

        echo "[$(date)] Running isochrone generation (attempt $((retry + 1)))..."
        Rscript scripts/generate_isochrones_parallel.R --bands "$band" 2>&1 | \
            tee "logs/isochrone_secondary_${band}min_$(date +%Y%m%d_%H%M%S).log" || true

        # Check how many IDs were generated
        n_ids=$(Rscript -e "
            rds <- 'artifacts/isochrones_secondary/isochrones_${band}min_consolidated.rds'
            if (file.exists(rds)) {
                d <- readRDS(rds)
                cat(length(unique(d\$id)))
            } else {
                cat(0)
            }
        " 2>/dev/null)

        echo "[$(date)] ${band}-min: $n_ids unique IDs generated"

        if [ "$n_ids" -gt 1700 ]; then
            echo "[$(date)] ${band}-min band complete with sufficient coverage ($n_ids/3407)"
            break
        fi

        retry=$((retry + 1))
        echo "[$(date)] Incomplete ($n_ids/3407), retrying in ${RETRY_DELAY}s..."
        sleep $RETRY_DELAY
    done
done

if [ "$INTERRUPTED" -eq 1 ]; then
    echo ""
    echo "[$(date)] Run interrupted. Resume with: bash scripts/resilient_isochrone_runner.sh ${BANDS}"
else
    echo ""
    echo "============================================"
    echo "[$(date)] All bands complete"
    echo "============================================"
fi
ls -lh artifacts/isochrones_secondary/*.rds 2>/dev/null
