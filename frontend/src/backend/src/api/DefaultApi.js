/**
 * ccfatigue
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 *
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */

import ApiClient from "../ApiClient";
import AppInfo from "../model/AppInfo";
import UnitInfo from "../model/UnitInfo";

/**
 * Default service.
 * @module api/DefaultApi
 * @version 0.1.0
 */
export default class DefaultApi {
  /**
   * Constructs a new DefaultApi.
   * @alias module:api/DefaultApi
   * @class
   * @param {module:ApiClient} [apiClient] Optional API client implementation to use,
   * default to {@link module:ApiClient#instance} if unspecified.
   */
  constructor(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;
  }

  /**
   * Callback function to receive the result of the getUnitsUnitsGet operation.
   * @callback module:api/DefaultApi~getUnitsUnitsGetCallback
   * @param {String} error Error message, if any.
   * @param {Array.<module:model/UnitInfo>} data The data returned by the service call.
   * @param {String} response The complete HTTP response.
   */

  /**
   * Get Units
   * Get All standardized units
   * @param {module:api/DefaultApi~getUnitsUnitsGetCallback} callback The callback function, accepting three arguments: error, data, response
   * data is of type: {@link Array.<module:model/UnitInfo>}
   */
  getUnitsUnitsGet(callback) {
    let postBody = null;

    let pathParams = {};
    let queryParams = {};
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = [UnitInfo];
    return this.apiClient.callApi(
      "/units",
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
      null,
      callback
    );
  }

  /**
   * Callback function to receive the result of the rootGet operation.
   * @callback module:api/DefaultApi~rootGetCallback
   * @param {String} error Error message, if any.
   * @param {module:model/AppInfo} data The data returned by the service call.
   * @param {String} response The complete HTTP response.
   */

  /**
   * Root
   * Get AppInfo
   * @param {module:api/DefaultApi~rootGetCallback} callback The callback function, accepting three arguments: error, data, response
   * data is of type: {@link module:model/AppInfo}
   */
  rootGet(callback) {
    let postBody = null;

    let pathParams = {};
    let queryParams = {};
    let headerParams = {};
    let formParams = {};

    let authNames = [];
    let contentTypes = [];
    let accepts = ["application/json"];
    let returnType = AppInfo;
    return this.apiClient.callApi(
      "/",
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
      null,
      callback
    );
  }
}
