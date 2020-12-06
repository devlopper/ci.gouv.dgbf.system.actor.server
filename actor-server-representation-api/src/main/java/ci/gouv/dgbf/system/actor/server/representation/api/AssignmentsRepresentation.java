package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@Path(AssignmentsRepresentation.PATH)
public interface AssignmentsRepresentation extends RepresentationEntity<AssignmentsDto> {
	
	@POST
	@Path(PATH_INITIALIZE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Initialiser les affectations")
	Response initialize();
	
	@POST
	@Path(PATH_APPLY_MODEL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Appliquer un modèle")
	Response applyModel(AssignmentsDto assignments,Filter.Dto filter,List<String> overridablesFieldsNames);
	
	@POST
	@Path(PATH_APPLY_MODEL_INTERNAL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Appliquer un modèle")
	Response applyModel(AssignmentsDto assignments);
	
	@POST
	@Path(PATH_SAVE_SCOPE_FUNCTIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des affectations")
	Response saveScopeFunctions(List<AssignmentsDto> collection);
	
	String PATH = "affectations";
	String PATH_INITIALIZE = "initialize";
	String PATH_APPLY_MODEL = "applyModel";
	String PATH_APPLY_MODEL_INTERNAL = "applyModelInternal";
	String PATH_SAVE_SCOPE_FUNCTIONS = "saveScopeFunctions";
	String TAG = "Affectations";
	
	static AssignmentsRepresentation getProxy() {
		return ProxyGetter.getInstance().get(AssignmentsRepresentation.class);
	}
}