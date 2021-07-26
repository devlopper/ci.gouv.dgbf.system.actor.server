package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.representation.server.OpenAPI;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;

@Path(PrivilegeOpenAPI.PATH)
@Tag(name = "Privilèges")
public interface PrivilegeOpenAPI extends OpenAPI {

	public static final String PATH = "open/privilege";
	
	public static final String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les privilèges",operationId = "obtenir_privileges")
	@APIResponses(value = {
			@APIResponse(description = "Privilèges obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des privilèges",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response get(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			);
}