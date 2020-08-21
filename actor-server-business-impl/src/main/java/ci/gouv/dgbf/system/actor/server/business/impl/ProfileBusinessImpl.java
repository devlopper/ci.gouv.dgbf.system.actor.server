package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ProfileBusinessImpl extends AbstractBusinessEntityImpl<Profile, ProfilePersistence> implements ProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateBefore__(Profile profile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(profile, properties, function);
		if(StringHelper.isBlank(profile.getIdentifier())) {
			if(StringHelper.isNotBlank(profile.getCode()) && profile.getType() != null)
				profile.setIdentifier((ProfileType.CODE_SYSTEME.equals(profile.getType().getCode()) ? "PFS" : "PFU")+"_"+profile.getCode());
		}
	}
	
	@Override
	protected void __listenExecuteCreateAfter__(Profile profile, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(profile, properties, function);
		if(CollectionHelper.isNotEmpty(profile.getPrivileges()))
			__inject__(ProfilePrivilegeBusiness.class).createMany(profile.getPrivileges().stream().map(x -> new ProfilePrivilege()
					.setIdentifier(profile.getIdentifier()+"_"+x.getIdentifier())
					.setProfile(profile).setPrivilege(x))
					.collect(Collectors.toList()));
	}
	
	@Override
	protected void __listenExecuteDeleteBefore__(Profile profile, Properties properties,BusinessFunctionRemover function) {
		super.__listenExecuteDeleteBefore__(profile, properties, function);
		Collection<ProfilePrivilege> profilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(List.of(profile.getCode()));
		if(CollectionHelper.isNotEmpty(profilePrivileges))
			__inject__(ProfilePrivilegeBusiness.class).deleteMany(profilePrivileges);
		
		Collection<ProfileFunction> profileFunctions = ProfileFunctionQuerier.getInstance().readByProfilesCodes(List.of(profile.getCode()));
		if(CollectionHelper.isNotEmpty(profileFunctions))
			__inject__(ProfileFunctionBusiness.class).deleteMany(profileFunctions);
	}
	
	@Override
	protected Boolean __isCallDeleteByInstanceOnDeleteByIdentifier__() {
		return Boolean.TRUE;
	}
}