import ApiClient from "../ApiClient";

/**
 * The QuasiStaticTest model module.
 * @module model/QuasiStaticTest
 * @version 0.1.0
 */
class QuasiStaticTest {
  /**
   * Constructs a new <code>QuasiStaticTest</code>.
   */
  constructor(
    specimenName,
    crackDisplacement,
    crackLoad,
    crackLength,
    displacement,
    load,
    strain,
    stress,
    specimenId,
    toughness,
    initial_crack_length,
    crack_fractureenergy_mbt,
    crack_fractureenergy_mcc, 
    crack_fractureenergy_ecm,
    young_modulus,
    poisson_ratio,
    g_init_mbt, 
    bridginglength_mbt,
    g_plateau_mbt,
    g_init_mcc, 
    bridginglength_mcc,
    g_plateau_mcc,
    g_init_ecm, 
    bridginglength_ecm,
    g_plateau_ecm,
  ) {
    QuasiStaticTest.initialize(
      this,
      specimenName,
      crackDisplacement,
      crackLoad,
      crackLength,
      displacement,
      load,
      strain,
      stress,
      specimenId,
      toughness,
      initial_crack_length,
      crack_fractureenergy_mbt,
      crack_fractureenergy_mcc,
      crack_fractureenergy_ecm,
      young_modulus,
      poisson_ratio,
      g_init_mbt,
      bridginglength_mbt,
      g_plateau_mbt,
      g_init_mcc, 
      bridginglength_mcc,
      g_plateau_mcc,
      g_init_ecm, 
      bridginglength_ecm,
      g_plateau_ecm,
    );
  }

  static initialize(
    obj,
    specimenName,
    crackDisplacement,
    crackLoad,
    crackLength,
    displacement,
    load,
    strain,
    stress,
    specimenId,
    toughness,
    initial_crack_length,
    crack_fractureenergy_mbt,
    crack_fractureenergy_mcc,
    crack_fractureenergy_ecm,
    young_modulus,
    poisson_ratio,
    g_init_mbt,
    bridginglength_mbt,
    g_plateau_mbt,
    g_init_mcc, 
    bridginglength_mcc,
    g_plateau_mcc,
    g_init_ecm, 
    bridginglength_ecm,
    g_plateau_ecm,
  ) {
    obj["specimen_name"] = specimenName;
    obj["crack_displacement"] = crackDisplacement;
    obj["crack_load"] = crackLoad;
    obj["crack_length"] = crackLength;
    obj["displacement"] = displacement;
    obj["load"] = load;
    obj["strain"] = strain;
    obj["stress"] = stress;
    obj["specimen_id"] = specimenId;
    obj["toughness"] = toughness;
    obj["initial_crack_length"] = initial_crack_length;
    obj["crack_fractureenergy_mbt"] = crack_fractureenergy_mbt;
    obj["crack_fractureenergy_mcc"] = crack_fractureenergy_mcc;
    obj["crack_fractureenergy_ecm"] = crack_fractureenergy_ecm;
    obj["young_modulus"] = young_modulus;
    obj["poisson_ratio"] = poisson_ratio;
    obj["g_init_mbt"] = g_init_mbt;
    obj["bridginglength_mbt"] = bridginglength_mbt;
    obj["g_plateau_mbt"] = g_plateau_mbt;
    obj["g_init_mcc"] = g_init_mcc;
    obj["bridginglength_mcc"] = bridginglength_mcc;
    obj["g_plateau_mcc"] = g_plateau_mcc;
    obj["g_init_ecm"] = g_init_ecm;
    obj["bridginglength_ecm"] = bridginglength_ecm;
    obj["g_plateau_ecm"] = g_plateau_ecm;
  }

  static constructFromObject(data, obj) {
    if (data) {
      console.log("Raw QuasiStaticTest data:", data); // useful logs
      obj = obj || new QuasiStaticTest();

      if (data.hasOwnProperty("specimen_name")) {
        obj["specimen_name"] = ApiClient.convertToType(data["specimen_name"], "String");
      }
      if (data.hasOwnProperty("crack_displacement")) {
        obj["crack_displacement"] = ApiClient.convertToType(data["crack_displacement"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_load")) {
        obj["crack_load"] = ApiClient.convertToType(data["crack_load"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_length")) {
        obj["crack_length"] = ApiClient.convertToType(data["crack_length"], ["Number"]);
      }
      if (data.hasOwnProperty("displacement")) {
        obj["displacement"] = ApiClient.convertToType(data["displacement"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("load")) {
        obj["load"] = ApiClient.convertToType(data["load"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("strain")) {
        obj["strain"] = ApiClient.convertToType(data["strain"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("stress")) {
        obj["stress"] = ApiClient.convertToType(data["stress"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("specimen_id")) {
        obj["specimen_id"] = ApiClient.convertToType(data["specimen_id"], "Number");
      }
      if (data.hasOwnProperty("toughness")) {
        obj["toughness"] = ApiClient.convertToType(data["toughness"], "Number");
      }
      if (data.hasOwnProperty("initial_crack_length")) {
        obj["initial_crack_length"] = ApiClient.convertToType(data["initial_crack_length"], "Number");
      }
      if (data.hasOwnProperty("crack_fractureenergy_mbt")) {
        obj["crack_fractureenergy_mbt"] = ApiClient.convertToType(data["crack_fractureenergy_mbt"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_fractureenergy_mcc")) {
        obj["crack_fractureenergy_mcc"] = ApiClient.convertToType(data["crack_fractureenergy_mcc"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_fractureenergy_ecm")) {
        obj["crack_fractureenergy_ecm"] = ApiClient.convertToType(data["crack_fractureenergy_ecm"], ["Number"]);
      }
      if (data.hasOwnProperty("young_modulus")) {
        obj["young_modulus"] = ApiClient.convertToType(data["young_modulus"], "Number");
      }
      if (data.hasOwnProperty("poisson_ratio")) {
        obj["poisson_ratio"] = ApiClient.convertToType(data["poisson_ratio"], "Number");
      }
      if (data.hasOwnProperty("g_init_mbt")) {
        obj["g_init_mbt"] = ApiClient.convertToType(data["g_init_mbt"], "Number");
      }
      if (data.hasOwnProperty("bridginglength_mbt")) {
        obj["bridginglength_mbt"] = ApiClient.convertToType(data["bridginglength_mbt"], "Number");
      }
      if (data.hasOwnProperty("g_plateau_mbt")) {
        obj["g_plateau_mbt"] = ApiClient.convertToType(data["g_plateau_mbt"], "Number");
      }
      if (data.hasOwnProperty("g_init_mcc")) {
        obj["g_init_mcc"] = ApiClient.convertToType(data["g_init_mcc"], "Number");
      }
      if (data.hasOwnProperty("bridginglength_mcc")) {
        obj["bridginglength_mcc"] = ApiClient.convertToType(data["bridginglength_mcc"], "Number");
      }
      if (data.hasOwnProperty("g_plateau_mcc")) {
        obj["g_plateau_mcc"] = ApiClient.convertToType(data["g_plateau_mcc"], "Number");
      }
            if (data.hasOwnProperty("g_init_ecm")) {
        obj["g_init_ecm"] = ApiClient.convertToType(data["g_init_ecm"], "Number");
      }
      if (data.hasOwnProperty("bridginglength_ecm")) {
        obj["bridginglength_ecm"] = ApiClient.convertToType(data["bridginglength_ecm"], "Number");
      }
      if (data.hasOwnProperty("g_plateau_ecm")) {
        obj["g_plateau_ecm"] = ApiClient.convertToType(data["g_plateau_ecm"], "Number");
      }
    }
    console.log("Constructed QuasiStaticTest object:", obj); // useful logs
    return obj;
  }
}

/** Properties */
QuasiStaticTest.prototype["specimen_name"] = undefined;
QuasiStaticTest.prototype["crack_displacement"] = undefined;
QuasiStaticTest.prototype["crack_load"] = undefined;
QuasiStaticTest.prototype["crack_length"] = undefined;
QuasiStaticTest.prototype["displacement"] = undefined;
QuasiStaticTest.prototype["load"] = undefined;
QuasiStaticTest.prototype["strain"] = undefined;
QuasiStaticTest.prototype["stress"] = undefined;
QuasiStaticTest.prototype["specimen_id"] = undefined;
QuasiStaticTest.prototype["toughness"] = undefined;
QuasiStaticTest.prototype["initial_crack_length"] = undefined;
QuasiStaticTest.prototype["crack_fractureenergy_mbt"] = undefined;
QuasiStaticTest.prototype["crack_fractureenergy_mcc"] = undefined;
QuasiStaticTest.prototype["crack_fractureenergy_ecm"] = undefined;
QuasiStaticTest.prototype["young_modulus"] = undefined;
QuasiStaticTest.prototype["poisson_ratio"] = undefined;
QuasiStaticTest.prototype["g_init_mbt"] = undefined;
QuasiStaticTest.prototype["bridginglength_mbt"] = undefined;
QuasiStaticTest.prototype["g_plateau_mbt"] = undefined;
QuasiStaticTest.prototype["g_init_mcc"] = undefined;
QuasiStaticTest.prototype["bridginglength_mcc"] = undefined;
QuasiStaticTest.prototype["g_plateau_mcc"] = undefined;
QuasiStaticTest.prototype["g_init_ecm"] = undefined;
QuasiStaticTest.prototype["bridginglength_ecm"] = undefined;
QuasiStaticTest.prototype["g_plateau_ecm"] = undefined;

export default QuasiStaticTest;
