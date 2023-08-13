#!/bin/bash

# Check if the process is running
if pgrep -f api.jar > /dev/null; then
    # If running, terminate it
    pkill -f api.jar
    echo "Terminated running application"
    exit 0  # Explicitly exit with a success status
else
    echo "No running application found"
    exit 0  # Explicitly exit with a success status
fi
