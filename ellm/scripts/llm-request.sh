#!/bin/bash

# Function to display help message
usage() {
  cat <<EOF
Usage: $0 -e <endpoint> -i <input> [-o <output>]

Options:
  -e  Endpoint (required)
  -i  Input data (required)
  -o  Output file path (optional, default: /tmp/speech.mp3)
  -h  Show this help message

Example:
  $0 -e my_endpoint -i my_input -o /path/to/output.mp3
EOF
}

# Default values
FILEPATH="/tmp/speech.mp3"

# Parse command-line arguments
while getopts ":e:i:o:h" opt; do
  case ${opt} in
    e )
      ENDPOINT=$OPTARG
      ;;
    i )
      INPUT=$OPTARG
      ;;
    o )
      FILEPATH=$OPTARG
      ;;
    h )
      usage
      exit 0
      ;;
    \? )
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
    : )
      echo "Option -$OPTARG requires an argument." >&2
      usage
      exit 1
      ;;
  esac
done

# Check if required arguments are provided
if [ -z "$ENDPOINT" ] || [ -z "$INPUT" ]; then
  echo "Error: Endpoint and input are required."
  usage
  exit 1
fi

# Define the URL and the JSON body
URL="http://localhost:5040/${ENDPOINT}/"
BODY=$(jq -n --arg input "$INPUT" --arg filepath "$FILEPATH" '{"data": {"input": $input, "filepath": $filepath}}')

echo "URL: $URL"
echo "BODY: $BODY"

# Make the POST request using curl
curl -X POST -H "Content-Type: application/json" -d "$BODY" "$URL"
