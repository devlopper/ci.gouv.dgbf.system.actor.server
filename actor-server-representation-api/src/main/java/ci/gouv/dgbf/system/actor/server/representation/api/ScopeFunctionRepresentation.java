package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;
import io.swagger.annotations.ApiOperation;

@Path(ScopeFunctionRepresentation.PATH)
public interface ScopeFunctionRepresentation extends RepresentationEntity<ScopeFunctionDto> {

	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Enregistrer des postes",tags = {TAG})
	Response save(Collection<ScopeFunctionDto> scopeFunctionDtos);
	
	@POST
	@Path(PATH_DERIVE_ALL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Dériver tous les postes",tags = {TAG})
	Response deriveAll();
	
	@POST
	@Path(PATH_CODIFY_ALL)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Codifier tous les postes",tags = {TAG})
	Response codifyAll();
	
	String PATH_SAVE = "save";
	String PATH_DERIVE_ALL = "deriveAll";
	String PATH_CODIFY_ALL = "codifyAll";
	String PATH = "scopefunction";

	String TAG = ScopeRepresentation.TAG;
	
	static ScopeFunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ScopeFunctionRepresentation.class);
	}
}