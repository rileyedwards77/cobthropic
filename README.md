# Cobthropic - COBOL Anthropic API Hello World

A simple COBOL programme that demonstrates connecting to the Anthropic API to send a "Hello world!" message and receive a response from Claude.

## Overview

This programme uses COBOL's system call functionality to execute curl commands for HTTP communication with the Anthropic API. It sends a basic chat message and displays the API response.

## Prerequisites

- **Docker** (recommended), or
- **COBOL Compiler**: GnuCOBOL (recommended), Micro Focus COBOL, or IBM COBOL
- **curl**: Must be installed and accessible from the system PATH (Docker image includes it)
- **Anthropic API Key**: You'll need a valid API key from Anthropic
- **Operating System**: Unix/Linux/macOS (Windows with appropriate shell)

## Installation & Setup

1. Clone this repository
2. Get your Anthropic API key:
   - Visit `https://console.anthropic.com/`
   - Create an account and generate an API key
   - Ensure you have sufficient credits
3. Do not hardcode your key in the source. This project reads the key from the environment variable `ANTHROPIC_API_KEY`.

## Build and Run (Docker - recommended)

- Build the image:
```bash
docker build -t cobthropic .
```

- Run the programme (provide your key at runtime):
```bash
docker run --rm \
  -e ANTHROPIC_API_KEY='your_real_key_here' \
  -v /Users/rileyedwards/Documents/Projects/cobthropic:/app \
  -w /app cobthropic \
  sh -lc 'cobc -free -x cobthropic.cob && ./cobthropic | cat'
```

## Local Compile/Run (non-Docker)

If you have GnuCOBOL and curl installed locally:
```bash
export ANTHROPIC_API_KEY='your_real_key_here'
cobc -free -x cobthropic.cob
./cobthropic
```

## Expected Output

The programme will display:
1. A header message
2. The built command (API key masked)
3. Status of the API call
4. The JSON response from the Anthropic API
5. A completion message

Example output:
```
=== Cobthropic: COBOL + Anthropic API ===
 
Command (key masked):
curl -X POST https://api.anthropic.com/v1/messages -H "Content-Type: application/json" -H "anthropic-version: 2023-06-01" -H "x-api-key: ****..." -d '{"model":"claude-3-5-sonnet-20241022","max_tokens":100,"messages":[{"role":"user","content":"Hello world!"}]}' > api_response.json 2>&1
Calling Anthropic API...
API call successful.
 
=== API Response ===
{"id":"msg_...","type":"message","role":"assistant","model":"claude-3-5-sonnet-20241022","content":[{"type":"text","text":"Hello! How can I help you today?"}],"stop_reason":"max_tokens"}
=== End Response ===
 
Programme completed successfully.
```

## Files Created

The programme creates a temporary file:
- `api_response.json` - Contains the raw API response

## Configuration Options

You can modify these variables in the WORKING-STORAGE SECTION of `cobthropic.cob`:

- `WS-JSON-PAYLOAD`: The message content and model parameters
- Model: Currently set to `claude-3-5-sonnet-20241022`

## Troubleshooting

### Common Issues

**"ERROR: Environment variable ANTHROPIC_API_KEY is not set."**
- Export your key or pass it via Docker `-e ANTHROPIC_API_KEY=...`

**"API call failed with return code: X"**
- Check your API key is valid and properly formatted (no extra spaces/newlines)
- Ensure you have internet connectivity
- Verify curl is installed (outside Docker): `curl --version`
- Check your Anthropic account has sufficient credits

**Compilation warnings**
- Ensure the source file ends with a newline
- Use `-free` with GnuCOBOL for free-format source: `cobc -free -x cobthropic.cob`

### Test your key with curl (outside COBOL)

```bash
export ANTHROPIC_API_KEY='your_real_key_here'

curl https://api.anthropic.com/v1/messages \
  -H "content-type: application/json" \
  -H "x-api-key: $ANTHROPIC_API_KEY" \
  -H "anthropic-version: 2023-06-01" \
  -d '{"model":"claude-3-5-sonnet-20241022","max_tokens":10,"messages":[{"role":"user","content":"Hi"}]}'
```

Or via Docker:
```bash
docker run --rm -e ANTHROPIC_API_KEY='your_real_key_here' cobthropic \
  sh -lc 'curl https://api.anthropic.com/v1/messages \
  -H "content-type: application/json" \
  -H "x-api-key: $ANTHROPIC_API_KEY" \
  -H "anthropic-version: 2023-06-01" \
  -d '\''{"model":"claude-3-5-sonnet-20241022","max_tokens":10,"messages":[{"role":"user","content":"Hi"}]}'\''' 
```

## Extending the Programme

This basic example can be extended to:
- Accept user input for custom messages
- Parse JSON responses more elegantly
- Handle different Anthropic models
- Implement error retry logic
- Create a more interactive chat interface

## Technical Notes

- Uses system calls to curl for HTTP communication (standard COBOL approach)
- Response handling via temporary file I/O
- Compatible with most COBOL implementations that support `CALL "SYSTEM"`
- JSON parsing is handled at display level (could be enhanced with proper JSON parsing)

## Security Considerations

- Never commit your API key to version control
- Use environment variables or env files to provide the API key
- The temporary response file contains API responses - ensure appropriate file permissions

## Licence

This is example code provided for educational purposes. Modify and use as needed for your projects.

## Support

For Anthropic API issues, consult the [Anthropic API documentation](https://docs.anthropic.com/).
For COBOL compiler issues, refer to your compiler's documentation.
