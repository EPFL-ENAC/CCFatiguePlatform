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

/**
 * The ExperimentModel model module.
 * @module model/ExperimentModel
 * @version 0.1.0
 */
class ExperimentModel {
  /**
   * Constructs a new <code>ExperimentModel</code>.
   * Defines how experiment is seen on the API
   * @alias module:model/ExperimentModel
   * @param id {Number}
   * @param researcher {String}
   * @param experimentType {String}
   * @param fracture {Boolean}
   */
  constructor(id, researcher, experimentType, fracture) {
    ExperimentModel.initialize(this, id, researcher, experimentType, fracture);
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(obj, id, researcher, experimentType, fracture) {
    obj["id"] = id;
    obj["researcher"] = researcher;
    obj["experiment_type"] = experimentType;
    obj["fracture"] = fracture;
  }

  /**
   * Constructs a <code>ExperimentModel</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/ExperimentModel} obj Optional instance to populate.
   * @return {module:model/ExperimentModel} The populated <code>ExperimentModel</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new ExperimentModel();

      if (data.hasOwnProperty("id")) {
        obj["id"] = ApiClient.convertToType(data["id"], "Number");
      }
      if (data.hasOwnProperty("laboratory")) {
        obj["laboratory"] = ApiClient.convertToType(
          data["laboratory"],
          "String"
        );
      }
      if (data.hasOwnProperty("researcher")) {
        obj["researcher"] = ApiClient.convertToType(
          data["researcher"],
          "String"
        );
      }
      if (data.hasOwnProperty("date")) {
        obj["date"] = ApiClient.convertToType(data["date"], "String");
      }
      if (data.hasOwnProperty("experiment_type")) {
        obj["experiment_type"] = ApiClient.convertToType(
          data["experiment_type"],
          "String"
        );
      }
      if (data.hasOwnProperty("fracture")) {
        obj["fracture"] = ApiClient.convertToType(data["fracture"], "Boolean");
      }
      if (data.hasOwnProperty("fracture_mode")) {
        obj["fracture_mode"] = ApiClient.convertToType(
          data["fracture_mode"],
          "String"
        );
      }
      if (data.hasOwnProperty("fatigue_test_type")) {
        obj["fatigue_test_type"] = ApiClient.convertToType(
          data["fatigue_test_type"],
          "String"
        );
      }
      if (data.hasOwnProperty("quasi_static_test_type")) {
        obj["quasi_static_test_type"] = ApiClient.convertToType(
          data["quasi_static_test_type"],
          "String"
        );
      }
      if (data.hasOwnProperty("temperature_test_type")) {
        obj["temperature_test_type"] = ApiClient.convertToType(
          data["temperature_test_type"],
          "String"
        );
      }
      if (data.hasOwnProperty("measuring_equipment")) {
        obj["measuring_equipment"] = ApiClient.convertToType(
          data["measuring_equipment"],
          "String"
        );
      }
      if (data.hasOwnProperty("reliability_level")) {
        obj["reliability_level"] = ApiClient.convertToType(
          data["reliability_level"],
          "Number"
        );
      }
      if (data.hasOwnProperty("control_mode")) {
        obj["control_mode"] = ApiClient.convertToType(
          data["control_mode"],
          "String"
        );
      }
      if (data.hasOwnProperty("publication_title")) {
        obj["publication_title"] = ApiClient.convertToType(
          data["publication_title"],
          "String"
        );
      }
      if (data.hasOwnProperty("publication_author")) {
        obj["publication_author"] = ApiClient.convertToType(
          data["publication_author"],
          "String"
        );
      }
      if (data.hasOwnProperty("publication_year")) {
        obj["publication_year"] = ApiClient.convertToType(
          data["publication_year"],
          "String"
        );
      }
      if (data.hasOwnProperty("publication_doi")) {
        obj["publication_doi"] = ApiClient.convertToType(
          data["publication_doi"],
          "String"
        );
      }
      if (data.hasOwnProperty("publication_images_repository")) {
        obj["publication_images_repository"] = ApiClient.convertToType(
          data["publication_images_repository"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_sample_type")) {
        obj["material_type_sample_type"] = ApiClient.convertToType(
          data["material_type_sample_type"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_fiber_material")) {
        obj["material_type_fiber_material"] = ApiClient.convertToType(
          data["material_type_fiber_material"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_fiber_form")) {
        obj["material_type_fiber_form"] = ApiClient.convertToType(
          data["material_type_fiber_form"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_area_density")) {
        obj["material_type_area_density"] = ApiClient.convertToType(
          data["material_type_area_density"],
          "Number"
        );
      }
      if (data.hasOwnProperty("material_type_resin")) {
        obj["material_type_resin"] = ApiClient.convertToType(
          data["material_type_resin"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_hardener")) {
        obj["material_type_hardener"] = ApiClient.convertToType(
          data["material_type_hardener"],
          "String"
        );
      }
      if (data.hasOwnProperty("material_type_mixing_ratio")) {
        obj["material_type_mixing_ratio"] = ApiClient.convertToType(
          data["material_type_mixing_ratio"],
          "String"
        );
      }
      if (data.hasOwnProperty("laminates_and_assemblies_curing_time")) {
        obj["laminates_and_assemblies_curing_time"] = ApiClient.convertToType(
          data["laminates_and_assemblies_curing_time"],
          "Number"
        );
      }
      if (data.hasOwnProperty("laminates_and_assemblies_curing_temperature")) {
        obj["laminates_and_assemblies_curing_temperature"] =
          ApiClient.convertToType(
            data["laminates_and_assemblies_curing_temperature"],
            "Number"
          );
      }
      if (data.hasOwnProperty("laminates_and_assemblies_curing_pressure")) {
        obj["laminates_and_assemblies_curing_pressure"] =
          ApiClient.convertToType(
            data["laminates_and_assemblies_curing_pressure"],
            "Number"
          );
      }
      if (data.hasOwnProperty("laminates_and_assemblies_fiber_volume_ratio")) {
        obj["laminates_and_assemblies_fiber_volume_ratio"] =
          ApiClient.convertToType(
            data["laminates_and_assemblies_fiber_volume_ratio"],
            "Number"
          );
      }
      if (data.hasOwnProperty("laminates_and_assemblies_stacking_sequence")) {
        obj["laminates_and_assemblies_stacking_sequence"] =
          ApiClient.convertToType(
            data["laminates_and_assemblies_stacking_sequence"],
            "String"
          );
      }
      if (data.hasOwnProperty("measurement_measuring_points")) {
        obj["measurement_measuring_points"] = ApiClient.convertToType(
          data["measurement_measuring_points"],
          "Number"
        );
      }
      if (data.hasOwnProperty("dic_analysis_subset_size")) {
        obj["dic_analysis_subset_size"] = ApiClient.convertToType(
          data["dic_analysis_subset_size"],
          "Number"
        );
      }
      if (data.hasOwnProperty("dic_analysis_step_size")) {
        obj["dic_analysis_step_size"] = ApiClient.convertToType(
          data["dic_analysis_step_size"],
          "Number"
        );
      }
    }
    return obj;
  }
}

/**
 * @member {Number} id
 */
ExperimentModel.prototype["id"] = undefined;

/**
 * @member {String} laboratory
 */
ExperimentModel.prototype["laboratory"] = undefined;

/**
 * @member {String} researcher
 */
ExperimentModel.prototype["researcher"] = undefined;

/**
 * @member {String} date
 */
ExperimentModel.prototype["date"] = undefined;

/**
 * @member {String} experiment_type
 */
ExperimentModel.prototype["experiment_type"] = undefined;

/**
 * @member {Boolean} fracture
 */
ExperimentModel.prototype["fracture"] = undefined;

/**
 * @member {String} fracture_mode
 */
ExperimentModel.prototype["fracture_mode"] = undefined;

/**
 * @member {String} fatigue_test_type
 */
ExperimentModel.prototype["fatigue_test_type"] = undefined;

/**
 * @member {String} quasi_static_test_type
 */
ExperimentModel.prototype["quasi_static_test_type"] = undefined;

/**
 * @member {String} temperature_test_type
 */
ExperimentModel.prototype["temperature_test_type"] = undefined;

/**
 * @member {String} measuring_equipment
 */
ExperimentModel.prototype["measuring_equipment"] = undefined;

/**
 * @member {Number} reliability_level
 */
ExperimentModel.prototype["reliability_level"] = undefined;

/**
 * @member {String} control_mode
 */
ExperimentModel.prototype["control_mode"] = undefined;

/**
 * @member {String} publication_title
 */
ExperimentModel.prototype["publication_title"] = undefined;

/**
 * @member {String} publication_author
 */
ExperimentModel.prototype["publication_author"] = undefined;

/**
 * @member {String} publication_year
 */
ExperimentModel.prototype["publication_year"] = undefined;

/**
 * @member {String} publication_doi
 */
ExperimentModel.prototype["publication_doi"] = undefined;

/**
 * @member {String} publication_images_repository
 */
ExperimentModel.prototype["publication_images_repository"] = undefined;

/**
 * @member {String} material_type_sample_type
 */
ExperimentModel.prototype["material_type_sample_type"] = undefined;

/**
 * @member {String} material_type_fiber_material
 */
ExperimentModel.prototype["material_type_fiber_material"] = undefined;

/**
 * @member {String} material_type_fiber_form
 */
ExperimentModel.prototype["material_type_fiber_form"] = undefined;

/**
 * @member {Number} material_type_area_density
 */
ExperimentModel.prototype["material_type_area_density"] = undefined;

/**
 * @member {String} material_type_resin
 */
ExperimentModel.prototype["material_type_resin"] = undefined;

/**
 * @member {String} material_type_hardener
 */
ExperimentModel.prototype["material_type_hardener"] = undefined;

/**
 * @member {String} material_type_mixing_ratio
 */
ExperimentModel.prototype["material_type_mixing_ratio"] = undefined;

/**
 * @member {Number} laminates_and_assemblies_curing_time
 */
ExperimentModel.prototype["laminates_and_assemblies_curing_time"] = undefined;

/**
 * @member {Number} laminates_and_assemblies_curing_temperature
 */
ExperimentModel.prototype["laminates_and_assemblies_curing_temperature"] =
  undefined;

/**
 * @member {Number} laminates_and_assemblies_curing_pressure
 */
ExperimentModel.prototype["laminates_and_assemblies_curing_pressure"] =
  undefined;

/**
 * @member {Number} laminates_and_assemblies_fiber_volume_ratio
 */
ExperimentModel.prototype["laminates_and_assemblies_fiber_volume_ratio"] =
  undefined;

/**
 * @member {String} laminates_and_assemblies_stacking_sequence
 */
ExperimentModel.prototype["laminates_and_assemblies_stacking_sequence"] =
  undefined;

/**
 * @member {Number} measurement_measuring_points
 */
ExperimentModel.prototype["measurement_measuring_points"] = undefined;

/**
 * @member {Number} dic_analysis_subset_size
 */
ExperimentModel.prototype["dic_analysis_subset_size"] = undefined;

/**
 * @member {Number} dic_analysis_step_size
 */
ExperimentModel.prototype["dic_analysis_step_size"] = undefined;

export default ExperimentModel;