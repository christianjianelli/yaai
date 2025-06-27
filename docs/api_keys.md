# yaai - ABAP AI tools - API Keys

API keys are required to authenticate requests to external AI services (such as OpenAI). The yaai ABAP AI tools provide a simple, optional mechanism for storing and retrieving API keys, but **do not offer a secure storage solution**. It is the responsibility of the user to implement a secure key management approach suitable for their environment.

## Basic API Key Management

The class `ycl_aai_api_key` provides basic methods to manage API keys in the database table `yaai_api_key`. The API key is stored in Base64-encoded form, but this is **not** secure.

### Methods

- `create( i_id, i_api_key )`: Stores a new API key for the given API identifier (e.g., 'OPENAI').
- `read( i_id )`: Retrieves the API key for the given identifier.
- `delete( i_id )`: Deletes the API key for the given identifier.

Example usage:

```abap
DATA(lo_key) = NEW ycl_aai_api_key( ).

" Create 
lo_key->create( i_id = 'OPENAI' i_api_key = 'sk-...' ).

" Read
DATA(l_api_key) = lo_key->read( i_id = 'OPENAI' ).

" Delete
lo_key->delete( i_id = 'OPENAI' ).
```

### Integration with Connection Object

When you instantiate a `ycl_aai_conn` connection object with an API name, it will automatically attempt to retrieve the API key using `ycl_aai_api_key`. You can also set the API key manually if you use a different storage mechanism.

```abap
DATA(lo_connection) = NEW ycl_aai_conn( i_api = 'OPENAI' ).
```

Repeating the previous example, but this time using the ABAP AI constants instead of hardcoded values.

```abap
DATA(lo_connection) = NEW ycl_aai_conn( i_api = yif_aai_const=>c_openai ).
```

You can implement your own version of the `yif_aai_api_key` interface. After creating a class that implements the interface `yif_aai_api_key`, pass an instance of your custom class to the ABAP AI Connection object. The connection object will then automatically use your implementation to retrieve the API key, allowing you to integrate with secure storage or other key management systems as needed.

```abap
"Instantiate your custom API key management class, for example a class named 'ycl_aai_my_custom_api_key', and pass it to the connection object as follows:
DATA(lo_key) = NEW ycl_aai_my_custom_api_key( ).

DATA(lo_connection) = NEW ycl_aai_conn( ).

lo_connection->set_api_key( i_o_api_key = lo_key ).
```