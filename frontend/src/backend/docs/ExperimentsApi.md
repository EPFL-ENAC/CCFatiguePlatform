# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                             | HTTP request         | Description     |
| ---------------------------------------------------------------------------------- | -------------------- | --------------- |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet) | **GET** /experiments | Get Experiments |

## getExperimentsExperimentsGet

> [ExperimentModel] getExperimentsExperimentsGet()

Get Experiments

Get all experiments

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
apiInstance.getExperimentsExperimentsGet((error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log("API called successfully. Returned data: " + data);
  }
});
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**[ExperimentModel]**](ExperimentModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
