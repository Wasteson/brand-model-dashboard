# brand_git_push.R

setwd("~/Desktop/wasteson/TrackSights/datating/looker/bigquery_3 auto merke og model analyse")

message("🔄 Pusher endringer til GitHub...")

system("git add .")
system("git commit -m 'Oppdaterte grafer og data'")
system("git push origin main")

message("✅ Ferdig! Alt er pushet til GitHub.")
