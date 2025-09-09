# Cobthropic - COBOL Anthropic API Hello World

A simple COBOL programme that demonstrates connecting to the Anthropic API to send a "Hello world!" message and receive a response from Claude.

## Overview

This programme uses COBOL's system call functionality to execute curl commands for HTTP communication with the Anthropic API. It sends a basic chat message and displays the API response.

## Prerequisites

- **COBOL Compiler**: GnuCOBOL (recommended), Micro Focus COBOL, or IBM COBOL
- **curl**: Must be installed and accessible from the system PATH
- **Anthropic API Key**: You'll need a valid API key from Anthropic
- **Operating System**: Unix/Linux/macOS (Windows with appropriate shell)

## Installation & Setup

1. **Clone or download** the `cobthropic.cob` file

2. **Get your Anthropic API key**:
   - Visit [https://console.anthropic.com/](https://console.anthropic.com/)
   - Create an account and generate an API key
   - Ensure you have sufficient credits

3. **Configure the API key**:
   Edit the COBOL file and replace this line:
   ```cobol
   01 WS-API-KEY         PIC X(100) VALUE 
       "your-api-key-here".
   ```
   
   With your actual API key:
   ```cobol
   01 WS-API-KEY         PIC X(100) VALUE 
       "sk-ant-api03-your-actual-key-here".
   ```

## Compilation

### Using GnuCOBOL (recommended)
```bash
cobc -x cobthropic.cob
```

### Using Micro Focus COBOL
```bash
cob cobthropic.cob
```

### Using IBM COBOL (z/OS)
```bash
cob2 cobthropic.cob
```

## Running the Programme

```bash
./cobthropic
```

## Expected Output

The programme will display:
1. A header message
2. Status of the API call
3. The complete JSON response from the Anthropic API
4. A completion message

Example output:
```
=== Cobthropic: COBOL + Anthropic API ===
 
Calling Anthropic API...
API call successful.
 
=== API Response ===
{"id":"msg_123abc","type":"message","role":"assistant","content":[{"type":"text","text":"Hello! How can I help you today?"}],"model":"claude-3-5-sonnet-20241022","stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":10,"output_tokens":12}}
=== End Response ===
 
Programme completed successfully.
```

## Files Created

The programme creates a temporary file:
- `api_response.json` - Contains the raw API response (automatically cleaned up)

## Configuration Options

You can modify these variables in the WORKING-STORAGE SECTION:

- **WS-API-KEY**: Your Anthropic API key
- **WS-JSON-PAYLOAD**: The message content and model parameters
- **Model**: Currently set to `claude-3-5-sonnet-20241022`

## Troubleshooting

### Common Issues

**"API call failed with return code: X"**
- Check your API key is valid and properly formatted
- Ensure you have internet connectivity
- Verify curl is installed: `curl --version`
- Check your Anthropic account has sufficient credits

**"Command not found" errors**
- Install curl: `sudo apt install curl` (Ubuntu/Debian) or `brew install curl` (macOS)
- Ensure your COBOL compiler is properly installed

**Compilation errors**
- Check your COBOL compiler supports the `CALL "SYSTEM"` statement
- Verify the source file encoding is correct
- Some older COBOL compilers may require syntax modifications

### Testing curl manually

Test your API connection outside of COBOL:
```bash
curl -X POST https://api.anthropic.com/v1/messages \
  -H "Content-Type: application/json" \
  -H "x-api-version: 2023-06-01" \
  -H "Authorization: Bearer your-api-key-here" \
  -d '{"model":"claude-3-5-sonnet-20241022","max_tokens":100,"messages":[{"role":"user","content":"Hello world!"}]}'
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
- Compatible with most COBOL implementations that support CALL "SYSTEM"
- JSON parsing is handled at display level (could be enhanced with proper JSON parsing)

## Security Considerations

- Never commit your API key to version control
- Consider using environment variables for the API key in production
- The temporary response file contains API responses - ensure appropriate file permissions

## Licence

This is example code provided for educational purposes. Modify and use as needed for your projects.

## Support

For Anthropic API issues, consult the [Anthropic API documentation](https://docs.anthropic.com/).
For COBOL compiler issues, refer to your compiler's documentation.
