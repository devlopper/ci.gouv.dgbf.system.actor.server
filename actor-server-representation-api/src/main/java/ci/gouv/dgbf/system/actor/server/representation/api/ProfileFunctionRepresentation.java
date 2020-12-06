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

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileFunctionDto;

@Path(ProfileFunctionRepresentation.PATH)
public interface ProfileFunctionRepresentation extends RepresentationEntity<ProfileFunctionDto> {
	
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	//@Operation(description = "Associer des profiles et des fonctions")
	Response save(Collection<ProfileFunctionDto> profileFunctionDtos);
	
	static ProfileFunctionRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ProfileFunctionRepresentation.class);
	}
	
	String PATH = "profilefunction";
	String PATH_SAVE = "save";
}