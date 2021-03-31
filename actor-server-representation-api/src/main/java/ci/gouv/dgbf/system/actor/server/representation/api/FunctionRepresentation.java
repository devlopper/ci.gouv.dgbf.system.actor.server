package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;

@Path(FunctionRepresentation.PATH)
public interface FunctionRepresentation extends RepresentationEntity<FunctionDto> {
	
	@GET
	@Path(PATH_GET_EXECUTION_HOLDERS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	Response getExecutionHolders();
	
	@GET
	@Path(PATH_GET_EXECUTION_HOLDERS_AND_ASSISTANTS)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	Response getExecutionHoldersAndAssistants();
	
	String PATH = "function";
	String PATH_GET_EXECUTION_HOLDERS = "titulairesexecution";
	String PATH_GET_EXECUTION_HOLDERS_AND_ASSISTANTS = "titulairesetassistantsexecution";
	
	String TAG = "Fonctions";
	
	static FunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(FunctionRepresentation.class);
	}
}