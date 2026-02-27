#!/bin/bash

# Auto-detect ValEngr root directory
VALENGR_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

"$VALENGR_ROOT/scripts/title.sh" "COMMANDS" "dark blue"
cd "$VALENGR_ROOT"

printf "\n"
printf "SYNCH COMMANDS\n"
printf "==============\n"
printf "./start_mls_synch.sh                # Start all MLS Downloads\n"
printf "          1. Verifies OrbStack/Docker is running (exits if not)\n"                                                                                                                               
printf "          2. Checks PostgreSQL container (starts it if needed)\n"                                                                                                                                 
printf "          3. Starts caffeinate to prevent Mac sleep\n"                                                                                                                                           
printf "          4. Shows available disk space\n"                                                                                                                                                       
printf "          5. Kills any duplicate processes before starting fresh\n"         
printf "./start_mls_sync.sh --status          # Check status\n"                       
printf "./start_mls_sync.sh --stop            # Stop all syncs\n"      
printf "./start_mls_sync.sh --photo_status    # Check detailed photo status \n"  
printf "./start_mls_sync.sh --restart_photos  # Restart just the photo downloads\n"   
printf "./start_mls_sync.sh –counties all"
printf "./start_mls_synch.sh -–counties alameda,contra_costa,monterey,santa_clara, santa_cruz,san_mateo,monterey\n"
printf "tail -f sync_*.out photos_*.out       # Monitor all logs \n"    

printf “\n”











 

