package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionExecutionImputationDto;
import io.swagger.annotations.ApiOperation;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeFunctionExecutionImputationRepresentation.PATH)
public interface ScopeFunctionExecutionImputationRepresentation extends RepresentationEntity<ScopeFunctionExecutionImputationDto> {
	
	@POST
	@Path(PATH_DERIVE_ALL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver toutes les assignations",tags = {TAG})
	Response deriveAll();
	
	String PATH = "scopefunctionexecutionimputation";	
	String PATH_DERIVE_ALL = "deriveAll";
	
	String TAG = ScopeFunctionRepresentation.TAG;
	
	static ScopeFunctionExecutionImputationRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ScopeFunctionExecutionImputationRepresentation.class);
	}
}
