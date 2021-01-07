package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;

@Path(ExecutionImputationRepresentation.PATH)
public interface ExecutionImputationRepresentation extends RepresentationEntity<ExecutionImputationDto> {
	
	@POST
	@Path(PATH_REFRES_HMATERIALIZED_VIEW)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Initialiser les affectations")
	Response refreshMaterializedView();
	
	String PATH = "executionimputation";
	String PATH_REFRES_HMATERIALIZED_VIEW = "rafraichirlavm";
	
	String TAG = ImputationRepresentation.TAG;
	
	static ExecutionImputationRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ExecutionImputationRepresentation.class);
	}
}