package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePrivilegePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

@ApplicationScoped
public class ProfilePrivilegeBusinessImpl extends AbstractBusinessEntityImpl<ProfilePrivilege, ProfilePrivilegePersistence> implements ProfilePrivilegeBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public void createFromPrivileges(Profile profile, Collection<Privilege> privileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("profile", profile);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("privileges", privileges);
		Collection<Privilege> availablePrivileges = PrivilegeQuerier.getInstance().readByProfilesCodesNotAssociated(List.of(profile.getCode()));
		if(CollectionHelper.isEmpty(availablePrivileges))
			return;
		availablePrivileges.retainAll(privileges);
		if(CollectionHelper.isEmpty(availablePrivileges))
			return;
		LogHelper.logInfo(String.format("%s available privilege(s) to add to profile %s",availablePrivileges.size(),profile.getCode()), getClass());
		Collection<ProfilePrivilege> profilePrivileges = availablePrivileges.stream().map(privilege -> new ProfilePrivilege().setProfile(profile).setPrivilege(privilege))
				.collect(Collectors.toList());
		createMany(profilePrivileges);
	}
	
	@Override @Transactional
	public void createFromProfiles(Profile profile,Collection<Profile> profiles) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("profile", profile);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readByProfilesCodes(profiles.stream().map(Profile::getCode).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(privileges))
			return;
		createFromPrivileges(profile, privileges);
	}

	@Override @Transactional
	public void createFromFunctions(Profile profile,Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("profile", profile);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		Collection<Profile> profiles = ProfileQuerier.getInstance().readByFunctionsCodes(functions.stream().map(Function::getCode).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(profiles))
			return;
		for(Function function : functions) {
			ProfileFunction profileFunction = ProfileFunctionQuerier.getInstance().readByProfileCodeByFunctionCode(profile.getCode(), function.getCode());
			if(profileFunction == null)
				__inject__(ProfileFunctionBusiness.class).create(new ProfileFunction().setProfile(profile).setFunction(function));
		}
		createFromProfiles(profile, profiles);
	}
}