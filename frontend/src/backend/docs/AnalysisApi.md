# Ccfatigue.AnalysisApi

All URIs are relative to _http://localhost_

| Method                                                                                            | HTTP request                    | Description       |
| ------------------------------------------------------------------------------------------------- | ------------------------------- | ----------------- |
| [**runSnCurveFileAnalysisSnCurveFilePost**](AnalysisApi.md#runSnCurveFileAnalysisSnCurveFilePost) | **POST** /analysis/snCurve/file | Run Sn Curve File |

## runSnCurveFileAnalysisSnCurveFilePost

> PageExperimentModel runSnCurveFileAnalysisSnCurveFilePost(methods, rRatios, file, opts)

Run Sn Curve File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let methods = [new Ccfatigue.SnCurveMethod()]; // [SnCurveMethod] |
let rRatios = [null]; // [Number] |
let file = "/path/to/file"; // File |
let opts = {
  page: 1, // Number |
  size: 50, // Number |
};
apiInstance.runSnCurveFileAnalysisSnCurveFilePost(
  methods,
  rRatios,
  file,
  opts,
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

| Name        | Type                                    | Description | Notes                      |
| ----------- | --------------------------------------- | ----------- | -------------------------- |
| **methods** | [**[SnCurveMethod]**](SnCurveMethod.md) |             |
| **rRatios** | [**[Number]**](Number.md)               |             |
| **file**    | **File**                                |             |
| **page**    | **Number**                              |             | [optional] [default to 1]  |
| **size**    | **Number**                              |             | [optional] [default to 50] |

### Return type

[**PageExperimentModel**](PageExperimentModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json
