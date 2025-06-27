# yaai - ABAP AI tools - Connection object

The `Connection` object manages communication between ABAP and external Large Language Model (LLM) APIs such as OpenAI and Ollama. It handles HTTP client creation, API Key retrieval, endpoint construction, and response handling.

### Configuration

**The configuration is optional**. Its main purpose is to simplify and streamline the use of supported APIs by allowing the `ABAP AI tools` to automatically resolve endpoints. Without configuration, you will need to provide connection details manually when instantiating the `Connection` object.

The Connection object retrieves base URLs for supported APIs from the SAP table `TVARVC`. 

For example, you can maintain the following parameters (see also [Configuration Parameters](config.md)):

| Parameter Name           | Description                                 |
|-------------------------|---------------------------------------------|
| YAAI_OPENAI_BASE_URL    | Base URL for OpenAI API endpoints           |
| YAAI_OLLAMA_BASE_URL    | Base URL for Ollama API endpoints           |

Example entries in TVARVC:

| NAME                   | VALUE                                 |
|------------------------|---------------------------------------|
| YAAI_OPENAI_BASE_URL   | https://api.openai.com/               |
| YAAI_OLLAMA_BASE_URL   | http://localhost:11434                |

### Automatic Instantiation

If the configuration parameters are properly maintained, the ABAP AI framework will automatically create and manage the `Connection` instance for you. In most cases, you do not need to instantiate the object manually—simply use the relevant ABAP AI APIs, and the connection will be handled internally.

### Manual Instantiation

You can instantiate the Connection object by specifying the API name:

```abap
DATA(lo_conn) = NEW ycl_aai_conn( i_api = 'OPENAI' ).
```

This will automatically:
- Set the base URL from TVARVC (e.g., `YAAI_OPENAI_BASE_URL`)
- Retrieve the API key using the API name
- Prepare the HTTP client for requests

You can also set or override the base URL and API key manually:

```abap
lo_conn->set_base_url( 'https://custom.api/' ).
```

### API Keys

API keys are managed via the `ycl_aai_api_keys` class and are set automatically during instantiation if available. You can also set them explicitly using the method `set_api_key` of the ycl_aai_conn class.

```abap
lo_conn->set_api_key( 'your-api-key' ).
```

---

### Notes

- The `Connection` object automatically sets the `Content-Type` header to `application/json`. By default, the SAP class **cl_http_client** appends `;charset=utf-8` to this header. The Nvidia API require the `Content-Type` to be exactly `application/json` and will return an error if any charset is present. The `suppress_content_type` method can be used to suppress the entire `Content-Type` header.

- When using the **SAP NetWeaver ABAP Developer Edition** to experiment with the ABAP AI tools, you may encounter SSL configuration challenges when connecting to external APIs such as OpenAI. To simplify connectivity, you can use a reverse proxy. For detailed instructions, see [Reverse Proxy Setup](reverse_proxy.md).