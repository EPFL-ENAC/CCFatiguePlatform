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
import ExperimentModel from "./ExperimentModel";

/**
 * The PageExperimentModel model module.
 * @module model/PageExperimentModel
 * @version 0.1.0
 */
class PageExperimentModel {
  /**
   * Constructs a new <code>PageExperimentModel</code>.
   * @alias module:model/PageExperimentModel
   * @param items {Array.<module:model/ExperimentModel>}
   * @param total {Number}
   * @param page {Number}
   * @param size {Number}
   */
  constructor(items, total, page, size) {
    PageExperimentModel.initialize(this, items, total, page, size);
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(obj, items, total, page, size) {
    obj["items"] = items;
    obj["total"] = total;
    obj["page"] = page;
    obj["size"] = size;
  }

  /**
   * Constructs a <code>PageExperimentModel</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/PageExperimentModel} obj Optional instance to populate.
   * @return {module:model/PageExperimentModel} The populated <code>PageExperimentModel</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new PageExperimentModel();

      if (data.hasOwnProperty("items")) {
        obj["items"] = ApiClient.convertToType(data["items"], [
          ExperimentModel,
        ]);
      }
      if (data.hasOwnProperty("total")) {
        obj["total"] = ApiClient.convertToType(data["total"], "Number");
      }
      if (data.hasOwnProperty("page")) {
        obj["page"] = ApiClient.convertToType(data["page"], "Number");
      }
      if (data.hasOwnProperty("size")) {
        obj["size"] = ApiClient.convertToType(data["size"], "Number");
      }
    }
    return obj;
  }
}

/**
 * @member {Array.<module:model/ExperimentModel>} items
 */
PageExperimentModel.prototype["items"] = undefined;

/**
 * @member {Number} total
 */
PageExperimentModel.prototype["total"] = undefined;

/**
 * @member {Number} page
 */
PageExperimentModel.prototype["page"] = undefined;

/**
 * @member {Number} size
 */
PageExperimentModel.prototype["size"] = undefined;

export default PageExperimentModel;
