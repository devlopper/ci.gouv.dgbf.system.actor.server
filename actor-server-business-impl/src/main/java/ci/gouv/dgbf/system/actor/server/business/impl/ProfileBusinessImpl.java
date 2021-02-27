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
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.security.keycloak.server.Role;
import org.cyk.utility.security.keycloak.server.RoleManager;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionRemover;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfilePrivilegeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfilePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfilePrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ProfileBusinessImpl extends AbstractBusinessEntityImpl<Profile, ProfilePersistence> implements ProfileBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override @Transactional
	public void createPrivileges(Collection<Profile> profiles, Collection<Privilege> privileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("privileges", privileges);
		Collection<ProfilePrivilege> creatablesProfilePrivileges = new ArrayList<>();
		Collection<ProfilePrivilege> existingProfilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(profiles.stream().map(x->x.getCode()).collect(Collectors.toList()));
		profiles.forEach(profile -> {
			Collection<Privilege> existingPrivileges = CollectionHelper.isEmpty(existingProfilePrivileges) ? null : existingProfilePrivileges.stream().filter(x->x.getProfile().equals(profile))
					.map(x->x.getPrivilege()).collect(Collectors.toList());
			Collection<Privilege> newPrivileges = CollectionHelper.isEmpty(existingPrivileges) ? privileges : privileges.stream().filter(x->!existingPrivileges.contains(x))
					.collect(Collectors.toList());
			newPrivileges = Privilege.getLeaves(newPrivileges);
			if(CollectionHelper.isNotEmpty(newPrivileges)) {				
				creatablesProfilePrivileges.addAll(newPrivileges.stream().map(privilege -> new ProfilePrivilege().setProfile(profile).setPrivilege(privilege)).collect(Collectors.toList()));
			}
		});
		if(CollectionHelper.isEmpty(creatablesProfilePrivileges))
			return;
		LogHelper.log(new LogHelper.Arguments()
				.setMessage(String.format("createPrivileges. #Profiles=%s. #Privilèges=%s. #Assignations=%s"
				, CollectionHelper.getSize(profiles),CollectionHelper.getSize(privileges),CollectionHelper.getSize(creatablesProfilePrivileges)))
				.setClassName(getClass().getName())
				.setMethodName("createPrivileges")
				);
		__inject__(ProfilePrivilegeBusiness.class).createMany(creatablesProfilePrivileges);
	}
	
	@Override @Transactional
	public void deletePrivileges(Collection<Profile> profiles, Collection<Privilege> privileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("privileges", privileges);
		Collection<ProfilePrivilege> oldProfilePrivileges = new ArrayList<>();
		Collection<ProfilePrivilege> existingProfilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(profiles.stream().map(x->x.getCode()).collect(Collectors.toList()));
		if(CollectionHelper.isEmpty(existingProfilePrivileges))
			return;
		/*Collection<Privilege> deletablePrivileges = null;
		for(Profile profile : profiles) {
			for(Privilege privilege : privileges) {
				PrivilegeQuerier.getInstance().read
			}
		}*/
		profiles.forEach(profile -> {
			/*for(ProfilePrivilege profilePrivilege : existingProfilePrivileges) {
				if(profilePrivilege.getProfile().equals(profile) && Privilege.getParent(privileges, profilePrivilege.getPrivilege()) == null) {
					oldProfilePrivileges.add(profilePrivilege);
				}
			}*/
			
			oldProfilePrivileges.addAll(existingProfilePrivileges.stream().filter(
					x->x.getProfile().equals(profile) && privileges.contains(x.getPrivilege())).collect(Collectors.toList()));
			
		});
		if(CollectionHelper.isEmpty(oldProfilePrivileges))
			return;
		LogHelper.log(new LogHelper.Arguments()
				.setMessage(String.format("deletePrivileges. #Profiles=%s. #Privilèges=%s. #Assignations=%s"
				, CollectionHelper.getSize(profiles),CollectionHelper.getSize(privileges),CollectionHelper.getSize(oldProfilePrivileges)))
				.setClassName(getClass().getName())
				.setMethodName("deletePrivileges")
				);
		__inject__(ProfilePrivilegeBusiness.class).deleteMany(oldProfilePrivileges);
	}
	
	@Override @Transactional
	public void savePrivileges(Collection<Profile> profiles, Collection<Privilege> creatablePrivileges,Collection<Privilege> deletablePrivileges) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("profiles", profiles);
		if(CollectionHelper.isEmpty(creatablePrivileges) && CollectionHelper.isEmpty(deletablePrivileges))
			throw new RuntimeException("creatable or deletable privileges required");
		LogHelper.log(new LogHelper.Arguments()
				.setMessage(String.format("savePrivileges. #Profiles=%s. #Privilèges soumis à création=%s. #Privilèges soumis à suppression=%s"
				, CollectionHelper.getSize(profiles),CollectionHelper.getSize(creatablePrivileges),CollectionHelper.getSize(deletablePrivileges)))
				.setClassName(getClass().getName())
				.setMethodName("savePrivileges")
				);
		if(CollectionHelper.isNotEmpty(creatablePrivileges))
			createPrivileges(profiles, creatablePrivileges);
		if(CollectionHelper.isNotEmpty(deletablePrivileges))
			deletePrivileges(profiles, deletablePrivileges);
	}
	
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
		if(profile.getType() != null && ProfileType.CODE_SYSTEME.equals(profile.getType().getCode()))
			throw new RuntimeException("Impossible de supprimer le profile système <<"+profile+">>");
		
		Collection<ProfilePrivilege> profilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(List.of(profile.getCode()));
		if(CollectionHelper.isNotEmpty(profilePrivileges))
			__inject__(ProfilePrivilegeBusiness.class).deleteMany(profilePrivileges);
		
		Collection<ProfileFunction> profileFunctions = ProfileFunctionQuerier.getInstance().readByProfilesCodes(List.of(profile.getCode()));
		if(CollectionHelper.isNotEmpty(profileFunctions))
			__inject__(ProfileFunctionBusiness.class).deleteMany(profileFunctions);
	}
	
	@Override @Transactional
	public void importFormKeycloakRoles() {
		Collection<Role> roles = RoleManager.getInstance().read();
		if(CollectionHelper.isEmpty(roles))
			return;
		Collection<Profile> profiles = null;
		for(Role role : roles) {
			Profile profile = __inject__(ProfilePersistence.class).readByBusinessIdentifier(role.getName());
			if(profile != null)
				continue;
			if(profiles == null)
				profiles = new ArrayList<Profile>();
			profiles.add(new Profile().setCode(role.getName()).setName(role.getName()));			
		}
		if(CollectionHelper.isEmpty(profiles))
			return;
		createMany(profiles);
	}

	@Override @Transactional
	public void exportToKeycloakRoles() {
		Collection<Profile> profiles = EntityReader.getInstance().readMany(Profile.class,ProfileQuerier.QUERY_IDENTIFIER_READ);
		if(CollectionHelper.isEmpty(profiles))
			return;
		RoleManager.getInstance().saveByNames(profiles.stream().map(x -> x.getCode()).collect(Collectors.toList()));
	}
	
	@Override
	protected Boolean __isCallDeleteByInstanceOnDeleteByIdentifier__() {
		return Boolean.TRUE;
	}
}