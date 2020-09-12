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

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;
import io.swagger.annotations.ApiOperation;

@Path(ProfileRepresentation.PATH)
public interface ProfileRepresentation extends RepresentationEntity<ProfileDto> {
	
	@POST
	@Path(PATH_SAVE_PRIVILEGES)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Enregistrer des privilèges",tags = {TAG})
	Response savePrivileges(Collection<ProfileDto> profiles);
	
	String PATH_SAVE_PRIVILEGES = "save_privileges";
	String PATH = "profile";

	String TAG = "Profiles";
	
	static ProfileRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ProfileRepresentation.class);
	}
}
