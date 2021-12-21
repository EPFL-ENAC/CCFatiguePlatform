# Ccfatigue.ExperimentsApi

All URIs are relative to _http://localhost_

| Method                                                                                                                                                                                                                                               | HTTP request                                                             | Description                                             |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ | ------------------------------------------------------- |
| [**getExperimentsExperimentsGet**](ExperimentsApi.md#getExperimentsExperimentsGet)                                                                                                                                                                   | **GET** /experiments                                                     | Get Experiments                                         |
| [**getFractureModeDistinctExperimentsFractureModeDistinctGet**](ExperimentsApi.md#getFractureModeDistinctExperimentsFractureModeDistinctGet)                                                                                                         | **GET** /experiments/fracture_mode/distinct                              | Get Fracture Mode Distinct                              |
| [**getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet**](ExperimentsApi.md#getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet) | **GET** /experiments/laminates_and_assemblies_stacking_sequence/distinct | Get Laminates And Assemblies Stacking Sequence Distinct |
| [**getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet**](ExperimentsApi.md#getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet)                                                     | **GET** /experiments/material_type_fiber_material/distinct               | Get Material Type Fiber Material Distinct               |
| [**getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet**](ExperimentsApi.md#getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet)                                                                                     | **GET** /experiments/material_type_resin/distinct                        | Get Material Type Resin Distinct                        |

## getExperimentsExperimentsGet

> PageExperimentModel getExperimentsExperimentsGet(opts)

Get Experiments

Get all experiments

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
let opts = {
  query: "query_example", // String |
  textSearch: "textSearch_example", // String |
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

| Name           | Type       | Description | Notes                      |
| -------------- | ---------- | ----------- | -------------------------- |
| **query**      | **String** |             | [optional]                 |
| **textSearch** | **String** |             | [optional]                 |
| **page**       | **Number** |             | [optional] [default to 1]  |
| **size**       | **Number** |             | [optional] [default to 50] |

### Return type

[**PageExperimentModel**](PageExperimentModel.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## getFractureModeDistinctExperimentsFractureModeDistinctGet

> [String] getFractureModeDistinctExperimentsFractureModeDistinctGet()

Get Fracture Mode Distinct

Get all distinct values for fracture_mode, sorted

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
apiInstance.getFractureModeDistinctExperimentsFractureModeDistinctGet(
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

This endpoint does not need any parameter.

### Return type

**[String]**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet

> [String] getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet()

Get Laminates And Assemblies Stacking Sequence Distinct

Get all distinct values for laminates_and_assemblies_stacking_sequence, sorted

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
apiInstance.getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet(
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

This endpoint does not need any parameter.

### Return type

**[String]**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet

> [String] getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet()

Get Material Type Fiber Material Distinct

Get all distinct values for material_type_fiber_material, sorted

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
apiInstance.getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet(
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

This endpoint does not need any parameter.

### Return type

**[String]**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

## getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet

> [String] getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet()

Get Material Type Resin Distinct

Get all distinct values for material_type_resin, sorted

### Example

```javascript
import Ccfatigue from "ccfatigue";

let apiInstance = new Ccfatigue.ExperimentsApi();
apiInstance.getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet(
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

This endpoint does not need any parameter.

### Return type

**[String]**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json
