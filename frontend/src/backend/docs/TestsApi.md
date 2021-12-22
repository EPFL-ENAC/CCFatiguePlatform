# Ccfatigue.TestsApi

All URIs are relative to _http://localhost_

| Method                                               | HTTP request   | Description |
| ---------------------------------------------------- | -------------- | ----------- |
| [**getTestsTestsGet**](TestsApi.md#getTestsTestsGet) | **GET** /tests | Get Tests   |

## getTestsTestsGet

> PageTestModel getTestsTestsGet(experimentId, opts)

Get Tests

Get all tests for specific experiment

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.TestsApi();
let experimentId = 56; // Number |
let opts = {
  page: 1, // Number |
  size: 50, // Number |
};
apiInstance.getTestsTestsGet(experimentId, opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log("API called successfully. Returned data: " + data);
  }
});
```

### Parameters

| Name             | Type       | Description | Notes                      |
| ---------------- | ---------- | ----------- | -------------------------- |
| **experimentId** | **Number** |             |
| **page**         | **Number** |             | [optional] [default to 1]  |
| **size**         | **Number** |             | [optional] [default to 50] |

### Return type

[**PageTestModel**](PageTestModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
