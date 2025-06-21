# ABAP AI Tools – Using a Reverse Proxy

When using the SAP Netweaver ABAP Developer Edition to experiment with the ABAP AI tools, you may encounter SSL configuration challenges when connecting to external APIs such as OpenAI.

To simplify connectivity, you can use a reverse proxy to expose remote HTTPS endpoints as local HTTP endpoints. This approach avoids complex SSL setup in your SAP system. We recommend using NGINX for this purpose.

## Setting Up a Reverse Proxy with NGINX

1. **Install NGINX**
   - Download and install NGINX from the [official website](https://nginx.org/en/download.html).
   - Refer to the [installation guide](https://nginx.org/en/docs/install.html) for your operating system.

2. **Configure the Reverse Proxy**
   - Edit the NGINX configuration file (usually `nginx.conf`).
   - Add a server block to proxy HTTP requests to the desired HTTPS API endpoint. 
   - The `/openai/` location allows you to send requests to `http://localhost/openai/`, which NGINX will forward to `https://api.openai.com/`.

    ```nginx
    server {
            listen       80;
            server_name  localhost;

            ...
            ...

            # OpenAI API Reverse Proxy
            # Add these lines to the NGINX config file
            location /openai/ {
                proxy_pass https://api.openai.com/;
                proxy_ssl_protocols TLSv1.2 TLSv1.3;
                proxy_ssl_ciphers 'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384'; # A strong, modern cipher list
                proxy_ssl_server_name on;
            }

            ...
            ...

    }
    ```

3. **Restart NGINX**
   - After saving your changes, restart NGINX to apply the new configuration.

4. **Update ABAP AI Tools Configuration**
   - Set the base URL in your configuration (e.g., `YAAI_OPENAI_BASE_URL`) to point to your local proxy, such as `http://localhost/openai/`.

    > **Note:** The configuration example above assumes that both SAP and NGINX are running on the same host (i.e., `localhost`). If NGINX is running on a different machine, update the base URL in your ABAP AI Tools configuration to use the appropriate hostname or IP address of the NGINX server (e.g., `http://<nginx-host>/openai/`).

## References
- [NGINX Reverse Proxy Guide](https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/)
- [NGINX Documentation](https://nginx.org/en/docs/)


