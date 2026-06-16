import ApiClient from "../ApiClient";
import ExperimentDataPreprocessed from "../model/ExperimentDataPreprocessed";
import FatigueTest from "../model/FatigueTest";
import PageExperimentModel from "../model/PageExperimentModel";
import QuasiStaticTest from "../model/QuasiStaticTest";

/**
 * Experiments service.
 * @module api/ExperimentsApi
 * @version 0.1.0
 */
export default class ExperimentsApi {
  constructor(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;
  }

  getExperimentsWithHttpInfo(opts) {
    opts = opts || {};
    let postBody = null;

    let pathParams = {};
    let queryParams = {
      query: opts["query"],
      text_search: opts["textSearch"],
      page: opts["page"],
      size: opts["size"],
    };
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = PageExperimentModel;

    return this.apiClient.callApi(
      "/experiments",
      "GET",
      pathParams,
      queryParams,
      headerParams,
      formParams,
      postBody,
      authNames,
      contentTypes,
      accepts,
      returnType,
      null
    );
  }

  getExperiments(opts) {
    return this.getExperimentsWithHttpInfo(opts).then(function (
      response_and_data
    ) {
      return response_and_data.data;
    });
  }

  getFatigueTestWithHttpInfo(experimentId, testId) {
    let postBody = null;
    if (experimentId === undefined || experimentId === null) {
      throw new Error(
        "Missing the required parameter 'experimentId' when calling getFatigueTest"
      );
    }
    if (testId === undefined || testId === null) {
      throw new Error(
        "Missing the required parameter 'testId' when calling getFatigueTest"
      );
    }

    let pathParams = {
      experiment_id: experimentId,
      test_id: testId,
    };
    let queryParams = {};
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = FatigueTest;

    return this.apiClient.callApi(
      "/experiments/{experiment_id}/fatigue/{test_id}",
      "GET",
      pathParams,
      queryParams,
      headerParams,
      formParams,
      postBody,
      authNames,
      contentTypes,
      accepts,
      returnType,
      null
    );
  }

  getFatigueTest(experimentId, testId) {
    return this.getFatigueTestWithHttpInfo(experimentId, testId).then(function (
      response_and_data
    ) {
      console.log("📡 RAW API RESPONSE FATIGUE TEST", response_and_data);
      return response_and_data.data;
    });
  }

  getFieldDistinctWithHttpInfo(field) {
    let postBody = null;
    if (field === undefined || field === null) {
      throw new Error(
        "Missing the required parameter 'field' when calling getFieldDistinct"
      );
    }

    let pathParams = {
      field: field,
    };
    let queryParams = {};
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = ["String"];

    return this.apiClient.callApi(
      "/experiments/{field}/distinct",
      "GET",
      pathParams,
      queryParams,
      headerParams,
      formParams,
      postBody,
      authNames,
      contentTypes,
      accepts,
      returnType,
      null
    );
  }

  getFieldDistinct(field) {
    return this.getFieldDistinctWithHttpInfo(field).then(function (
      response_and_data
    ) {
      return response_and_data.data;
    });
  }

  getQuasiStaticTestWithHttpInfo(experimentId, testId) {
    let postBody = null;

    if (experimentId === undefined || experimentId === null) {
      throw new Error("Missing 'experimentId'");
    }
    if (testId === undefined || testId === null) {
      throw new Error("Missing 'testId'");
    }

    let pathParams = {
      experiment_id: experimentId,
      test_id: testId,
    };
    let queryParams = {};
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = QuasiStaticTest;

    return this.apiClient.callApi(
      "/experiments/{experiment_id}/quasi-static/{test_id}",
      "GET",
      pathParams,
      queryParams,
      headerParams,
      formParams,
      postBody,
      authNames,
      contentTypes,
      accepts,
      returnType,
      null
    );
  }

  getQuasiStaticTest(experimentId, testId) {
    return this.getQuasiStaticTestWithHttpInfo(experimentId, testId).then(
      function (response_and_data) {
        console.log("📡 RAW API RESPONSE QUASI STATIC TEST", response_and_data);
        return response_and_data.data;
      }
    );
  }

  postDataPreprocessCheckWithHttpInfo(file) {
    let postBody = null;
    if (file === undefined || file === null) {
      throw new Error(
        "Missing the required parameter 'file' when calling postDataPreprocessCheck"
      );
    }

    let pathParams = {};
    let queryParams = {};
    let headerParams = {};
    let formParams = { file: file };

    let authNames = [];
    let contentTypes = ["multipart/form-data"];
    let accepts = ["application/json"];
    let returnType = ExperimentDataPreprocessed;

    return this.apiClient.callApi(
      "/experiments/data_preprocess_check",
      "POST",
      pathParams,
      queryParams,
      headerParams,
      formParams,
      postBody,
      authNames,
      contentTypes,
      accepts,
      returnType,
      null
    );
  }

  postDataPreprocessCheck(file) {
    return this.postDataPreprocessCheckWithHttpInfo(file).then(function (
      response_and_data
    ) {
      return response_and_data.data;
    });
  }
}
