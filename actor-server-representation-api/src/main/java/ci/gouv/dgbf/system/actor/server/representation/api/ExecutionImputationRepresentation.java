package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;

@Path(ExecutionImputationRepresentation.PATH)
public interface ExecutionImputationRepresentation extends RepresentationEntity<ExecutionImputationDto> {
	
	String PATH = "executionimputation";
	
	String TAG = ImputationRepresentation.TAG;
	
	static ExecutionImputationRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ExecutionImputationRepresentation.class);
	}
}