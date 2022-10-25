# Ccfatigue.AnalysisApi

All URIs are relative to _http://localhost_

| Method                                                                                                                    | HTTP request                          | Description             |
| ------------------------------------------------------------------------------------------------------------------------- | ------------------------------------- | ----------------------- |
| [**runCycleCountingFileAnalysisCycleCountingFilePost**](AnalysisApi.md#runCycleCountingFileAnalysisCycleCountingFilePost) | **POST** /analysis/cycleCounting/file | Run Cycle Counting File |
| [**runSnCurveFileAnalysisSnCurveFilePost**](AnalysisApi.md#runSnCurveFileAnalysisSnCurveFilePost)                         | **POST** /analysis/snCurve/file       | Run Sn Curve File       |

## runCycleCountingFileAnalysisCycleCountingFilePost

> File runCycleCountingFileAnalysisCycleCountingFilePost(method, file)

Run Cycle Counting File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let method = new Ccfatigue.CycleCountingMethod(); // CycleCountingMethod |
let file = "/path/to/file"; // File |
apiInstance
  .runCycleCountingFileAnalysisCycleCountingFilePost(method, file)
  .then(
    (data) => {
      console.log("API called successfully. Returned data: " + data);
    },
    (error) => {
      console.error(error);
    }
  );
```

### Parameters

| Name       | Type                           | Description | Notes |
| ---------- | ------------------------------ | ----------- | ----- |
| **method** | [**CycleCountingMethod**](.md) |             |
| **file**   | **File**                       |             |

### Return type

**File**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

## runSnCurveFileAnalysisSnCurveFilePost

> SnCurveResult runSnCurveFileAnalysisSnCurveFilePost(methods, rRatios, file)

Run Sn Curve File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let methods = [new Ccfatigue.SnCurveMethod()]; // [SnCurveMethod] |
let rRatios = [null]; // [Number] |
let file = "/path/to/file"; // File |
apiInstance.runSnCurveFileAnalysisSnCurveFilePost(methods, rRatios, file).then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
```

### Parameters

| Name        | Type                                    | Description | Notes |
| ----------- | --------------------------------------- | ----------- | ----- |
| **methods** | [**[SnCurveMethod]**](SnCurveMethod.md) |             |
| **rRatios** | [**[Number]**](Number.md)               |             |
| **file**    | **File**                                |             |

### Return type

[**SnCurveResult**](SnCurveResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json
