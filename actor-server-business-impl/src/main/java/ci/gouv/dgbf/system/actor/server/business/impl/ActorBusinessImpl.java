package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.ProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileTypePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ActorBusinessImpl extends AbstractBusinessEntityImpl<Actor, ActorPersistence> implements ActorBusiness,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateAfter__(Actor actor, Properties properties, BusinessFunctionCreator function) {
		super.__listenExecuteCreateAfter__(actor, properties, function);
		//we instantiate user profile
		Profile profile = new Profile().setType(__inject__(ProfileTypePersistence.class).readByBusinessIdentifier(ProfileType.CODE_UTILISATEUR));
		profile.setCode("profile_"+actor.getCode());
		profile.setName("Profile de "+actor.getFirstName());
		//we collect predefined privileges from system profiles based on given functions
		if(CollectionHelper.isNotEmpty(actor.getFunctions())) {			
			Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readByProfilesTypesCodesByFunctionsCodesOrderByCodeAscending(List.of(ProfileType.CODE_SYSTEM)
					, actor.getFunctions().stream().map(x -> x.getCode()).collect(Collectors.toSet()));			
			if(CollectionHelper.isNotEmpty(privileges))								
				profile.getPrivileges(Boolean.TRUE).addAll(privileges);
		}
		__inject__(ProfileBusiness.class).create(profile);
		__inject__(ActorProfileBusiness.class).create(new ActorProfile().setActor(actor).setProfile(profile));
	}
}