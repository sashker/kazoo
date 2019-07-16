# Functions

## About Functions

Functions enable dynamic call processing based on CouchDB view.

#### Schema

Functions are subscriptions to allowed events that, when the event occurs, the event data is sent to the function set in the function document.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | Is the function enabled and running | `boolean()` | `true` | `false` |  
`function_js` | Serverless function in Javascript | `string()` |   | `true` |  
`name` | A descriptive name for the function. | `string()` |   | `true` | `supported`
`trigger` | The trigger event for a request being made to 'callback_uri'. | `string()` |   | `true` | `supported`



## Fetch available functions on the system

Depending on the version of the KAZOO system running, the available functions may differ. Use this API to query the system for available functions.

> GET /v2/functions

```shell
curl -v -X GET \
    -H "Content-Type:application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN} \
    http://{SERVER}:8000/v2/functions
```

## Get sample payloads of all function events

> GET /v2/functions/samples

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/functions/samples
```

### Example

**Request:**

```shell
curl -H 'Content-Type: application/json' 'http://{SERVER}:8000/v2/functions/samples'
```
## List functions

> GET /v2/accounts/{ACCOUNT_ID}/functions

Any functions with *disable_reason* in the summary has been auto-disabled.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Create function

> PUT /v2/accounts/{ACCOUNT_ID}/functions

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "name": "New Func"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

## Get details of the function

> GET /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Edit function

> POST /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "name": "New Func"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Patch function

> PATCH /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

You can also patch an existing function:

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"enabled":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Delete a function

> DELETE /v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions/{FUNCTION_ID}
```

## Enable auto-disabled hooks in bulk

Enable an account's functions

> PATCH /v2/accounts/{ACCOUNT_ID}/functions

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"re-enable":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/functions
```

