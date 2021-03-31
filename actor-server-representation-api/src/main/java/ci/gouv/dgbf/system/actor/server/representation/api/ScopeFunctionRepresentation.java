package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;

@Path(ScopeFunctionRepresentation.PATH)
public interface ScopeFunctionRepresentation extends RepresentationEntity<ScopeFunctionDto> {

	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Enregistrer des postes")
	Response save(Collection<ScopeFunctionDto> scopeFunctionDtos);
	
	@POST
	@Path(PATH_DERIVE_ALL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Dériver tous les postes")
	Response deriveAll();
	
	@POST
	@Path(PATH_DERIVE_BY_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Dériver les postes par fonctions")
	Response deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	@POST
	@Path(PATH_DERIVE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Dériver les postes par fonctions")
	Response deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	@POST
	@Path(PATH_CODIFY_ALL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Codifier tous les postes")
	Response codifyAll();
	
	@POST
	@Path(PATH_CODIFY_BY_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Codifier les postes par fonctions")
	Response codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	@POST
	@Path(PATH_CODIFY_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Codifier les postes par fonctions")
	Response codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	@POST
	@Path(PATH_DELETE_BY_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer les postes par fonctions")
	Response deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers);
	
	@POST
	@Path(PATH_DELETE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer les postes par fonctions")
	Response deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers);
	
	@GET
	@Path(PATH_COMPUTE_CREDIT_MANAGER_HOLDER_NAME)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer les postes par fonctions")
	Response computeCreditManagerHolderNameByAdministrativeUnitIdentifier(@QueryParam("unite_administrative") String administrativeUnitIdentifier);
	
	@GET
	@Path(PATH_COMPUTE_AUTHORIZING_OFFICER_HOLDER_NAME)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer les postes par fonctions")
	Response computeAuthorizingOfficerHolderNameByBudgetSpecializationUnitIdentifierByLocalityIdentifier(@QueryParam("unite_specialisation_budget") String budgetSpecializationUnitIdentifier
			,@QueryParam("localite") String localityIdentifier);
	
	@GET
	@Path(PATH_GET_BY_SCOPE_IDENTIFIER_BY_FUNCTION_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Supprimer les postes par fonctions")
	Response getByScopeIdentifierByFunctionIdentifier(@QueryParam("domaine") String scopeIdentifier,@QueryParam("fonction") String functionIdentifier);
	
	String PATH_GET_BY_SCOPE_IDENTIFIER_BY_FUNCTION_IDENTIFIER = "getByScopeIdentifierByFunctionIdentifier";
	String PATH_COMPUTE_CREDIT_MANAGER_HOLDER_NAME = "computeCreditManagerHolderName";
	String PATH_COMPUTE_AUTHORIZING_OFFICER_HOLDER_NAME = "computeAuthorizingOfficerHolderName";
	String PATH_SAVE = "save";
	String PATH_DERIVE_ALL = "deriveAll";
	String PATH_DERIVE_BY_FUNCTIONS_IDENTIFIERS = "deriveByFunctionsIdentifiers";
	String PATH_DERIVE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	String PATH_CODIFY_ALL = "codifyAll";
	String PATH_CODIFY_BY_FUNCTIONS_IDENTIFIERS = "codifyByFunctionsIdentifiers";
	String PATH_CODIFY_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	String PATH_DELETE_BY_FUNCTIONS_IDENTIFIERS = "deleteByFunctionsIdentifiers";
	String PATH_DELETE_HOLDERS_AND_ASSISTANTS_BY_HOLDERS_FUNCTIONS_IDENTIFIERS = "deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers";
	String PATH = "scopefunction";

	String TAG = ScopeRepresentation.TAG;
	
	static ScopeFunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ScopeFunctionRepresentation.class);
	}
}