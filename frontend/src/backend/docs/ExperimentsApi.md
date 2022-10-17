# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                                                                                               | HTTP request                                                | Description                |
| ---------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- | -------------------------- |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet)                                                                   | **GET** /experiments                                        | Get Experiments            |
| [**getFatigueTestExperimentsExperimentIdFatigueTestIdGet**](ExperimentsApi.md#getFatigueTestExperimentsExperimentIdFatigueTestIdGet)                 | **GET** /experiments/{experiment_id}/fatigue/{test_id}      | Get Fatigue Test           |
| [**getFieldDistinctExperimentsFieldDistinctGet**](ExperimentsApi.md#getFieldDistinctExperimentsFieldDistinctGet)                                     | **GET** /experiments/{field}/distinct                       | Get Field Distinct         |
| [**getQuasiStaticTestExperimentsExperimentIdQuasiStaticTestIdGet**](ExperimentsApi.md#getQuasiStaticTestExperimentsExperimentIdQuasiStaticTestIdGet) | **GET** /experiments/{experiment_id}/quasi-static/{test_id} | Get Quasi Static Test      |
| [**postDataPreprocessCheckExperimentsDataPreprocessCheckPost**](ExperimentsApi.md#postDataPreprocessCheckExperimentsDataPreprocessCheckPost)         | **POST** /experiments/data_preprocess_check                 | Post Data Preprocess Check |

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
apiInstance.getExperimentsExperimentsGet(opts).then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
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

## getFatigueTestExperimentsExperimentIdFatigueTestIdGet

> FatigueTest getFatigueTestExperimentsExperimentIdFatigueTestIdGet(experimentId, testId)

Get Fatigue Test

Return test result data

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let experimentId = 56; // Number |
let testId = 56; // Number |
apiInstance
  .getFatigueTestExperimentsExperimentIdFatigueTestIdGet(experimentId, testId)
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

| Name             | Type       | Description | Notes |
| ---------------- | ---------- | ----------- | ----- |
| **experimentId** | **Number** |             |
| **testId**       | **Number** |             |

### Return type

[**FatigueTest**](FatigueTest.md)

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
apiInstance.getFieldDistinctExperimentsFieldDistinctGet(field).then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
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

## getQuasiStaticTestExperimentsExperimentIdQuasiStaticTestIdGet

> QuasiStaticTest getQuasiStaticTestExperimentsExperimentIdQuasiStaticTestIdGet(experimentId, testId)

Get Quasi Static Test

Return quasi static test

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let experimentId = 56; // Number |
let testId = 56; // Number |
apiInstance
  .getQuasiStaticTestExperimentsExperimentIdQuasiStaticTestIdGet(
    experimentId,
    testId
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

| Name             | Type       | Description | Notes |
| ---------------- | ---------- | ----------- | ----- |
| **experimentId** | **Number** |             |
| **testId**       | **Number** |             |

### Return type

[**QuasiStaticTest**](QuasiStaticTest.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## postDataPreprocessCheckExperimentsDataPreprocessCheckPost

> ExperimentDataPreprocessed postDataPreprocessCheckExperimentsDataPreprocessCheckPost(file)

Post Data Preprocess Check

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let file = "/path/to/file"; // File |
apiInstance
  .postDataPreprocessCheckExperimentsDataPreprocessCheckPost(file)
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

| Name     | Type     | Description | Notes |
| -------- | -------- | ----------- | ----- |
| **file** | **File** |             |

### Return type

[**ExperimentDataPreprocessed**](ExperimentDataPreprocessed.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json
