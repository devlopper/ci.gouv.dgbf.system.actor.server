package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@Path(AssignmentsRepresentation.PATH)
public interface AssignmentsRepresentation extends RepresentationEntity<AssignmentsDto> {
	
	@POST
	@Path(PATH_INITIALIZE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Initialiser les affectations")
	Response initialize(@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_DERIVE_ALL_VALUES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Initialiser les affectations")
	Response deriveAllValues(@QueryParam(QUERY_PARAMETER_NAME_HOLDERS_SETTABLE)Boolean holdersSettable
			,@QueryParam(QUERY_PARAMETER_NAME_ASSISTANTS_SETTABLE)Boolean assistantsSettable,@QueryParam(QUERY_PARAMETER_OVERRIDABLE) Boolean overridable
			,@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_APPLY_MODEL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Appliquer un modèle")
	Response applyModel(AssignmentsDto assignments,Filter.Dto filter,List<String> overridablesFieldsNames,@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_APPLY_MODEL_INTERNAL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Appliquer un modèle")
	Response applyModel(AssignmentsDto assignments,@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_SAVE_SCOPE_FUNCTIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des affectations")
	Response saveScopeFunctions(List<AssignmentsDto> collection);
	
	@POST
	@Path(PATH_CLEAN)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des affectations")
	Response clean(@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_IMPORT)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des affectations")
	Response import_(@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@POST
	@Path(PATH_EXPORT)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des affectations")
	Response export(@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	String PATH = "affectations";
	String PATH_DERIVE_ALL_VALUES = "derivertouteslesvaleurs";
	String PATH_INITIALIZE = "initialize";
	String PATH_APPLY_MODEL = "applyModel";
	String PATH_APPLY_MODEL_INTERNAL = "applyModelInternal";
	String PATH_SAVE_SCOPE_FUNCTIONS = "saveScopeFunctions";
	String PATH_CLEAN = "effacer";
	String PATH_IMPORT = "importer";
	String PATH_EXPORT = "exporter";
	String TAG = "Affectations";
	
	String QUERY_PARAMETER_OVERRIDABLE = "ecraser";
	String QUERY_PARAMETER_NAME_HOLDERS_SETTABLE = "titulaire";
	String QUERY_PARAMETER_NAME_ASSISTANTS_SETTABLE = "assistant";
	
	
	static AssignmentsRepresentation getProxy() {
		return ProxyGetter.getInstance().get(AssignmentsRepresentation.class);
	}
}