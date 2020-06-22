package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

@ApplicationScoped
public class ProfileBusinessImpl extends AbstractBusinessEntityImpl<Profile, ProfilePersistence> implements ProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateAfter__(Profile profile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(profile, properties, function);
		if(CollectionHelper.isNotEmpty(profile.getPrivileges()))
			__inject__(ProfilePrivilegeBusiness.class).createMany(profile.getPrivileges().stream().map(x -> new ProfilePrivilege().setProfile(profile).setPrivilege(x))
					.collect(Collectors.toList()));
	}	
}