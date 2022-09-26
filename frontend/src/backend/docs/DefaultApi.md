# Ccfatigue.DefaultApi

All URIs are relative to _http://localhost_

| Method                                                 | HTTP request   | Description |
| ------------------------------------------------------ | -------------- | ----------- |
| [**getUnitsUnitsGet**](DefaultApi.md#getUnitsUnitsGet) | **GET** /units | Get Units   |
| [**rootGet**](DefaultApi.md#rootGet)                   | **GET** /      | Root        |

## getUnitsUnitsGet

> [UnitInfo] getUnitsUnitsGet()

Get Units

Get All standardized units

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.DefaultApi();
apiInstance.getUnitsUnitsGet().then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**[UnitInfo]**](UnitInfo.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## rootGet

> AppInfo rootGet()

Root

Get AppInfo

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.DefaultApi();
apiInstance.rootGet().then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**AppInfo**](AppInfo.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
