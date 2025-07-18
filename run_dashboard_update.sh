#!/bin/bash

echo "▶️ Kjører brand_model_update.R..."
Rscript brand_model_update.R

echo "✅ Ferdig med visualisering. Pusher til GitHub..."
Rscript brand_git_push.R

echo "🎉 Ferdig!"

