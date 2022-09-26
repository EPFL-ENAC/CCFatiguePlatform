# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                                                                                       | HTTP request                                | Description                |
| -------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------- | -------------------------- |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet)                                                           | **GET** /experiments                        | Get Experiments            |
| [**getFieldDistinctExperimentsFieldDistinctGet**](ExperimentsApi.md#getFieldDistinctExperimentsFieldDistinctGet)                             | **GET** /experiments/{field}/distinct       | Get Field Distinct         |
| [**getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet**](ExperimentsApi.md#getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet)     | **GET** /experiments/tests_dashboard_plots  | Get Tests Dashboard Plots  |
| [**postDataPreprocessCheckExperimentsDataPreprocessCheckPost**](ExperimentsApi.md#postDataPreprocessCheckExperimentsDataPreprocessCheckPost) | **POST** /experiments/data_preprocess_check | Post Data Preprocess Check |

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

## getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet

> DashboardPlots getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet(opts)

Get Tests Dashboard Plots

Return the 4 Bokeh plots used in Test Dashboard Note: as we don&#39;t have real data yet, we hard code things this so it will render the 10 first tests of the experiment 1 (only experiment we have) : + experiment&#x3D;1 + 1&lt;tests_ids&lt;10 then we mascarade test_id field so that it looks like to be matching the one asked for.

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let opts = {
  experimentId: 56, // Number |
  testIds: [null], // [Number] |
};
apiInstance.getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet(opts).then(
  (data) => {
    console.log("API called successfully. Returned data: " + data);
  },
  (error) => {
    console.error(error);
  }
);
```

### Parameters

| Name             | Type                      | Description | Notes      |
| ---------------- | ------------------------- | ----------- | ---------- |
| **experimentId** | **Number**                |             | [optional] |
| **testIds**      | [**[Number]**](Number.md) |             | [optional] |

### Return type

[**DashboardPlots**](DashboardPlots.md)

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
