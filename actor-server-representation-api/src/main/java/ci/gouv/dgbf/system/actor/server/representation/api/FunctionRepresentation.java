package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;

@Path(FunctionRepresentation.PATH)
public interface FunctionRepresentation extends RepresentationEntity<FunctionDto> {
	/*
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Enregistrer des fonctions",tags = {TAG})
	Response save(Collection<FunctionDto> functionDtos);
	*/
	//String PATH_SAVE = "save";
	String PATH = "function";
	
	String TAG = "Fonctions";
	
	static FunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(FunctionRepresentation.class);
	}
}