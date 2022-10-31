# Ccfatigue.AnalysisApi

All URIs are relative to _http://localhost_

| Method                                                                                                                            | HTTP request                            | Description               |
| --------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------- | ------------------------- |
| [**runCldFileAnalysisCldFilePost**](AnalysisApi.md#runCldFileAnalysisCldFilePost)                                                 | **POST** /analysis/cld/file             | Run Cld File              |
| [**runCycleCountingFileAnalysisCycleCountingFilePost**](AnalysisApi.md#runCycleCountingFileAnalysisCycleCountingFilePost)         | **POST** /analysis/cycleCounting/file   | Run Cycle Counting File   |
| [**runDamageSummationFileAnalysisDamageSummationFilePost**](AnalysisApi.md#runDamageSummationFileAnalysisDamageSummationFilePost) | **POST** /analysis/damageSummation/file | Run Damage Summation File |
| [**runFatigueFailureFileAnalysisFatigueFailureFilePost**](AnalysisApi.md#runFatigueFailureFileAnalysisFatigueFailureFilePost)     | **POST** /analysis/fatigueFailure/file  | Run Fatigue Failure File  |
| [**runSnCurveFileAnalysisSnCurveFilePost**](AnalysisApi.md#runSnCurveFileAnalysisSnCurveFilePost)                                 | **POST** /analysis/snCurve/file         | Run Sn Curve File         |

## runCldFileAnalysisCldFilePost

> File runCldFileAnalysisCldFilePost(method, file)

Run Cld File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let method = new Ccfatigue.CldMethod(); // CldMethod |
let file = "/path/to/file"; // File |
apiInstance.runCldFileAnalysisCldFilePost(method, file).then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
```

### Parameters

| Name       | Type                 | Description | Notes |
| ---------- | -------------------- | ----------- | ----- |
| **method** | [**CldMethod**](.md) |             |
| **file**   | **File**             |             |

### Return type

**File**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

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

## runDamageSummationFileAnalysisDamageSummationFilePost

> File runDamageSummationFileAnalysisDamageSummationFilePost(method, sncFile, cycFile)

Run Damage Summation File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let method = new Ccfatigue.DamageSummationMethod(); // DamageSummationMethod |
let sncFile = "/path/to/file"; // File |
let cycFile = "/path/to/file"; // File |
apiInstance
  .runDamageSummationFileAnalysisDamageSummationFilePost(
    method,
    sncFile,
    cycFile
  )
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

| Name        | Type                             | Description | Notes |
| ----------- | -------------------------------- | ----------- | ----- |
| **method**  | [**DamageSummationMethod**](.md) |             |
| **sncFile** | **File**                         |             |
| **cycFile** | **File**                         |             |

### Return type

**File**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

## runFatigueFailureFileAnalysisFatigueFailureFilePost

> File runFatigueFailureFileAnalysisFatigueFailureFilePost(method, snModel, desirableAngle, offAxisAngle, xFile, yFile, fFile)

Run Fatigue Failure File

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.AnalysisApi();
let method = new Ccfatigue.FatigueFailureMethod(); // FatigueFailureMethod |
let snModel = new Ccfatigue.SnModel(); // SnModel |
let desirableAngle = 3.4; // Number |
let offAxisAngle = 3.4; // Number |
let xFile = "/path/to/file"; // File |
let yFile = "/path/to/file"; // File |
let fFile = "/path/to/file"; // File |
apiInstance
  .runFatigueFailureFileAnalysisFatigueFailureFilePost(
    method,
    snModel,
    desirableAngle,
    offAxisAngle,
    xFile,
    yFile,
    fFile
  )
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

| Name               | Type                            | Description | Notes |
| ------------------ | ------------------------------- | ----------- | ----- |
| **method**         | [**FatigueFailureMethod**](.md) |             |
| **snModel**        | [**SnModel**](.md)              |             |
| **desirableAngle** | **Number**                      |             |
| **offAxisAngle**   | **Number**                      |             |
| **xFile**          | **File**                        |             |
| **yFile**          | **File**                        |             |
| **fFile**          | **File**                        |             |

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
