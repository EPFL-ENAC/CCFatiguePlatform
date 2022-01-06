# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                                                           | HTTP request                          | Description        |
| ---------------------------------------------------------------------------------------------------------------- | ------------------------------------- | ------------------ |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet)                               | **GET** /experiments                  | Get Experiments    |
| [**getFieldDistinctExperimentsFieldDistinctGet**](ExperimentsApi.md#getFieldDistinctExperimentsFieldDistinctGet) | **GET** /experiments/{field}/distinct | Get Field Distinct |

## getExperimentsExperimentsGet

> PageExperimentModel getExperimentsExperimentsGet(opts)

Get Experiments

Get all experiments

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let opts = {
  query: "''", // String |
  textSearch: "''", // String |
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

| Name           | Type       | Description | Notes                              |
| -------------- | ---------- | ----------- | ---------------------------------- |
| **query**      | **String** |             | [optional] [default to &#39;&#39;] |
| **textSearch** | **String** |             | [optional] [default to &#39;&#39;] |
| **page**       | **Number** |             | [optional] [default to 1]          |
| **size**       | **Number** |             | [optional] [default to 50]         |

### Return type

[**PageExperimentModel**](PageExperimentModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## getFieldDistinctExperimentsFieldDistinctGet

> [String] getFieldDistinctExperimentsFieldDistinctGet(field)

Get Field Distinct

Get all distinct values for field column, sorted

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let field = new Ccfatigue.ExperimentFieldNames(); // ExperimentFieldNames |
apiInstance.getFieldDistinctExperimentsFieldDistinctGet(
  field,
  (error, data, response) => {
    if (error) {
      console.error(error);
    } else {
      console.log("API called successfully. Returned data: " + data);
    }
  }
);
```

### Parameters

| Name      | Type                            | Description | Notes |
| --------- | ------------------------------- | ----------- | ----- |
| **field** | [**ExperimentFieldNames**](.md) |             |

### Return type

**[String]**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
