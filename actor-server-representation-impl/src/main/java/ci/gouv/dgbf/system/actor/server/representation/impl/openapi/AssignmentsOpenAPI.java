package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.representation.server.OpenAPI;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.enums.ParameterStyle;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionRepresentation;

@Path(AssignmentsOpenAPI.PATH)
@Tag(name = "Affectations")
public interface AssignmentsOpenAPI extends OpenAPI {

	public static final String PATH = "open/affectations";
	
	public static final String OPERATION_CREATE_SCOPE_FUNCTION = "creer-poste";
	@POST
	@Path(OPERATION_CREATE_SCOPE_FUNCTION)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Créer un poste",operationId = "creer_poste")
	@APIResponses(value = {
			@APIResponse(description = "Poste créé",responseCode = "201", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de la création du poste",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response createByScopeIdentifierByCategoryCode(
			@Parameter(allowEmptyValue = false,description = "Identifiant du domaine",example = "01",name = "identifiant_domaine",required = true)
			@QueryParam("identifiant_domaine") String scopeIdentifier
			
			,@Parameter(description = ScopeFunctionRepresentation.DESCRIPTION_CATEGORY_CODE,example = ScopeType.CODE_UA,name = ScopeFunctionRepresentation.PARAMETER_CATEGORY_CODE
			,allowEmptyValue = false,required = true
			,schema = @Schema(enumeration = {
					ScopeFunction.CATEGORY_CODE_G1					
					,ScopeFunction.CATEGORY_CODE_O2,ScopeFunction.CATEGORY_CODE_O3,ScopeFunction.CATEGORY_CODE_O9					
					,ScopeFunction.CATEGORY_CODE_C1,ScopeFunction.CATEGORY_CODE_C2,ScopeFunction.CATEGORY_CODE_C3
				}))
			@QueryParam(ScopeFunctionRepresentation.PARAMETER_CATEGORY_CODE) String scopeTypeCode
			
			,@Parameter(allowEmptyValue = false,description = ScopeFunctionRepresentation.DESCRIPTION_NAME,example = ScopeFunctionRepresentation.EXAMPLE_NAME
			,name = ScopeFunctionRepresentation.PARAMETER_NAME,required = true)
			@QueryParam(ScopeFunctionRepresentation.PARAMETER_NAME) String name
			
			,@Parameter(allowEmptyValue = false,description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME
			,name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode			
			);
	
	public static final String OPERATION_IMPORT = "importer";
	@POST
	@Path(OPERATION_IMPORT)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({ MediaType.TEXT_PLAIN})
	@Operation(description = "Importer des lignes budgétaires",operationId = "importer_lignes_budgétaires")
	@APIResponses(value = {
			@APIResponse(description = "Lignes budgétaires importées",responseCode = "200", content = @Content(mediaType = MediaType.TEXT_PLAIN))
			,@APIResponse(description = "Erreur lors de l'importer des lignes budgétaires",responseCode = "500", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	})
	public Response importNewsAndDeriveValuesByReferencedIdentifiersAndExport(
			@Parameter(allowEmptyValue = false,description = "Identifiants lignes budgétaires",example = "01",name = "identifiants_lignes_budgetaires",required = true
			,style = ParameterStyle.DEFAULT)
			@QueryParam("identifiants_lignes_budgetaires") List<String> referencedIdentifiers
			
			,@Parameter(allowEmptyValue = false,description = ActorRepresentation.DESCRIPTION_USER_NAME,example = ActorRepresentation.EXAMPLE_USER_NAME
			,name = ActorRepresentation.PARAMETER_USER_NAME,required = true)
			@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode			
			);
}