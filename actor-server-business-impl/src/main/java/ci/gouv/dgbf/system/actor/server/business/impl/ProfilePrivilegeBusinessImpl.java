package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.ArrayList;
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
	public void createFromPrivileges(Collection<Profile> profiles, Collection<Privilege> privileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("privileges", privileges);
		profiles.forEach(profile -> {
			createFromPrivileges(profile, privileges);
		});
	}
	
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
	public void createFromProfiles(Collection<Profile> profiles, Collection<Profile> profilesOfPrivileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profilesOfPrivileges", profilesOfPrivileges);
		Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readByProfilesCodes(profilesOfPrivileges.stream().map(Profile::getCode).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(privileges))
			return;
		createFromPrivileges(profiles, privileges);
	}
	
	@Override @Transactional
	public void createFromProfiles(Profile profile,Collection<Profile> profilesOfPrivileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("profile", profile);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profilesOfPrivileges", profilesOfPrivileges);
		createFromProfiles(List.of(profile), profilesOfPrivileges);
	}

	@Override
	public void createFromFunctions(Collection<Profile> profiles, Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		Collection<Profile> profilesOfFunctions = ProfileQuerier.getInstance().readByFunctionsCodes(functions.stream().map(Function::getCode).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(profilesOfFunctions))
			return;
		Collection<ProfileFunction> profileFunctions = new ArrayList<ProfileFunction>();
		profiles.forEach(profile -> {
			functions.forEach(function -> {
				ProfileFunction profileFunction = ProfileFunctionQuerier.getInstance().readByProfileCodeByFunctionCode(profile.getCode(), function.getCode());
				if(profileFunction == null)
					profileFunctions.add(new ProfileFunction().setProfile(profile).setFunction(function));
			});
		});
		__inject__(ProfileFunctionBusiness.class).createMany(profileFunctions);
		createFromProfiles(profiles, profilesOfFunctions);
	}
	
	@Override @Transactional
	public void createFromFunctions(Profile profile,Collection<Function> functions) {
		ThrowableHelper.throwIllegalArgumentExceptionIfNull("profile", profile);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("functions", functions);
		createFromFunctions(List.of(profile), functions);
	}
}