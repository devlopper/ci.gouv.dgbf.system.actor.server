package ci.gouv.dgbf.system.actor.server.representation.api;

import java.io.Serializable;

import javax.ws.rs.Path;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.__kernel__.representation.EntitySaver;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.representation.entities.ProfilePrivilegeDto;

@Path(ProfilePrivilegeSaver.PATH)
public interface ProfilePrivilegeSaver extends EntitySaver<ProfilePrivilegeDto> {

	/**/

	public static abstract class AbstractImpl extends EntitySaver.AbstractImpl<ProfilePrivilegeDto> implements ProfilePrivilegeSaver,Serializable {	
		@Override
		protected Class<ProfilePrivilegeDto> getRepresentationEntityClass() {
			return ProfilePrivilegeDto.class;
		}
	}
	
	/**/
	
	static ProfilePrivilegeSaver getInstance() {
		return Helper.getInstance(ProfilePrivilegeSaver.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	String PATH = ProfilePrivilegeRepresentation.PATH+"/saver/execute";
	
	static ProfilePrivilegeSaver getProxy() {
		return ProxyGetter.getInstance().get(ProfilePrivilegeSaver.class);
	}
}