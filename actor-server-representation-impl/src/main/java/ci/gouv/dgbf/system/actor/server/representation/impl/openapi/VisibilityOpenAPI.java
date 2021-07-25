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
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeRepresentation;

@Path(VisibilityOpenAPI.PATH)
@Tag(name = "Visibilités")
public interface VisibilityOpenAPI extends OpenAPI {

	public static final String PATH = "open/visibilite";
	
	public static final String OPERATION_GET = "obtenir";
	@GET
	@Path(OPERATION_GET)
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir les domaines visibles",operationId = "obtenir_domaines_visibles")
	@APIResponses(value = {
			@APIResponse(description = "Domaines visibles obtenues",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
			//,@APIResponse(description = "Le code du type de domaine est inconnu",responseCode = "400", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			//,@APIResponse(description = "Le nom d'utilisateur est inconnu",responseCode = "400", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de l'obtention des domaines visibles",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response get(
			@Parameter(description = ScopeRepresentation.DESCRIPTION_TYPE_CODE,example = ScopeRepresentation.EXAMPLE_TYPE_CODE,name = ScopeRepresentation.PARAMETER_TYPE_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {ScopeType.CODE_CATEGORIE_BUDGET,ScopeType.CODE_CATEGORIE_ACTIVITE,ScopeType.CODE_AB,ScopeType.CODE_SECTION,ScopeType.CODE_UA
					,ScopeType.CODE_USB,ScopeType.CODE_ACTION,ScopeType.CODE_ACTIVITE,ScopeType.CODE_IMPUTATION}))
			@QueryParam(ScopeRepresentation.PARAMETER_TYPE_CODE) String typeCode
			
			,@Parameter(description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME,name = ActorRepresentation.PARAMETER_USER_NAME
			,allowEmptyValue = false,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_PAGEABLE,example = ScopeRepresentation.EXAMPLE_PAGEABLE,name = ScopeRepresentation.PARAMETER_PAGEABLE
			,allowEmptyValue = true)
			@QueryParam(ScopeRepresentation.PARAMETER_PAGEABLE) Boolean pageable
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_FIRST_TUPLE_INDEX,example = ScopeRepresentation.EXAMPLE_FIRST_TUPLE_INDEX,name = ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX)
			@QueryParam(ScopeRepresentation.PARAMETER_FIRST_TUPLE_INDEX) Integer firstTupleIndex
			
			,@Parameter(description = ScopeRepresentation.DESCRIPTION_NUMBER_OF_TUPLES,example = ScopeRepresentation.EXAMPLE_NUMBER_OF_TUPLES,name = ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES)
			@QueryParam(ScopeRepresentation.PARAMETER_NUMBER_OF_TUPLES) Integer numberOfTuples
			);
	
	/**/
	
	public static enum TypeCode {
		SECTION
	}
}