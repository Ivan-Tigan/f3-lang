## Builtins

Req fetch Res.

```f3
(
    (
        1 a post;
        url "https://httpbin.org/post";
        params (param1 = 1. param2 = 3.);
        body (
            user = (
                name = "John".
                age = 30.
                address = (street = "123 Main St". city = "New York".).
            ).
            settings = (
                enabled = true.
                theme = "dark".
            ).
        ).
        headers = (
            "Content-Type" = "application/json".
            "X-Custom-Header" = "test-value".
        ).
    ) fetch Res.
) => (

).
```
