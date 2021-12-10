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
import org.cyk.utility.server.representation.RepresentationEntity;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.enums.ParameterStyle;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameters;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeFunctionDto;

@Path(RequestScopeFunctionRepresentation.PATH)
public interface RequestScopeFunctionRepresentation extends RepresentationEntity<RequestScopeFunctionDto> {

	@POST
	@Path(PATH_UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	Response updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(@QueryParam("postes_identifiants")List<String> scopeFunctionsIdentifiers
			,@QueryParam("acteur_code")String actorCode);
	
	@POST
	@Path("notification-specimen-signature")
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Notifier les spécimens de signature")
	@Parameters(value = {
			@Parameter(name = "Identifiants",allowEmptyValue = false,description = "Identifiants",required = true,example = "001",style = ParameterStyle.SIMPLE)
	})
	Response notifySignatureSpecimen(@QueryParam("identifiant") List<String> identifiers);
	
	String PATH = "requestscopefunction";
	String PATH_UPDATE_GRANTED_TO_FALSE_WHERE_TRUE_BY_SCOPE_FUNCTIONS_IDENTIFIERS = "desaccorder";

	static RequestScopeFunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestScopeFunctionRepresentation.class);
	}
}