package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
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

@Path(VisibilityOpenAPI.PATH)
@Tag(name = "Visibilités")
public interface VisibilityOpenAPI extends OpenAPI {

	public static final String PATH = "open/visibilite";
	
	public static final String OPERATION_GET_SECTIONS = "obtenir_sections";
	@POST
	@Path(OPERATION_GET_SECTIONS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les sections visibles",operationId = "obtenir_sections_visibles")
	@APIResponses(value = {
			@APIResponse(description = "Sections visibles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			,@APIResponse(description = "Erreur lors de l'obtention des sections visibles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response getSections(
			@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME)
			@FormParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			);
	
}