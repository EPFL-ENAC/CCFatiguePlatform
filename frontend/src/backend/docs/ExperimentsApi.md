# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                             | HTTP request         | Description     |
| ---------------------------------------------------------------------------------- | -------------------- | --------------- |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet) | **GET** /experiments | Get Experiments |

## getExperimentsExperimentsGet

> PageExperimentModel getExperimentsExperimentsGet(opts)

Get Experiments

Get all experiments

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let opts = {
  page: 1, // Number |
  size: 50, // Number |
};
apiInstance.getExperimentsExperimentsGet(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log("API called successfully. Returned data: " + data);
  }
});
```

### Parameters

| Name     | Type       | Description | Notes                      |
| -------- | ---------- | ----------- | -------------------------- |
| **page** | **Number** |             | [optional] [default to 1]  |
| **size** | **Number** |             | [optional] [default to 50] |

### Return type

[**PageExperimentModel**](PageExperimentModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
