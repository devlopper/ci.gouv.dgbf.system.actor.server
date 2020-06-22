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

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfilePrivilegeDto;
import io.swagger.annotations.ApiOperation;

@Path(ProfilePrivilegeRepresentation.PATH)
public interface ProfilePrivilegeRepresentation extends RepresentationEntity<ProfilePrivilegeDto> {
	
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "save",tags = {"save"})
	Response save(Collection<ProfilePrivilegeDto> profilePrivilegeDtos);
	
	static ProfilePrivilegeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ProfilePrivilegeRepresentation.class);
	}
	
	String PATH = "profileprivilege";
	String PATH_SAVE = "save";
}