package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;
import io.swagger.annotations.ApiOperation;

@Path(ExecutionImputationRepresentation.PATH)
public interface ExecutionImputationRepresentation extends RepresentationEntity<ExecutionImputationDto> {
	
	@POST
	@Path(PATH_SAVE_SCOPE_FUNCTIONS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Enregistrer les fonctions",tags = {TAG})
	Response saveScopeFunctions(Collection<ExecutionImputationDto> executionImputationDtos);
	
	String PATH = "executionimputation";
	String PATH_SAVE_SCOPE_FUNCTIONS = "saveScopeFunctions";
	
	String TAG = ImputationRepresentation.TAG;
	
	static ExecutionImputationRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ExecutionImputationRepresentation.class);
	}
}