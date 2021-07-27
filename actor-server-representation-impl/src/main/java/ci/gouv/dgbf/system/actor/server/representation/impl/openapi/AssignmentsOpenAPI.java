package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.util.List;

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

@Path(AssignmentsOpenAPI.PATH)
@Tag(name = "Affectations")
public interface AssignmentsOpenAPI extends OpenAPI {

	public static final String PATH = "open/affectations";
	
	public static final String OPERATION_IMPORT = "import";
	@POST
	@Path(OPERATION_IMPORT)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Importer des lignes budgétaires",operationId = "import")
	@APIResponses(value = {
			@APIResponse(description = "Lignes budgétaires importées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de l'importer des lignes budgétaires",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response importNewsAndDeriveValuesByIdentifiersAndExport(
			@Parameter(allowEmptyValue = false,description = "Identifiants lignes budgétaires",example = "01",name = "identifiants_lignes_budgetaires",required = true)
			@FormParam("identifiants_lignes_budgetaires") List<String> identifiers
			
			,@Parameter(allowEmptyValue = false,description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME
			,name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@FormParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode			
			);
}