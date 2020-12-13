package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.security.keycloak.server.User;
import org.cyk.utility.security.keycloak.server.UserManager;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;
import org.cyk.utility.server.persistence.PersistenceFunctionCreator;
import org.cyk.utility.server.persistence.PersistenceFunctionRemover;

import ci.gouv.dgbf.system.actor.server.persistence.api.ProfileFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ApplicationScoped
public class ProfileFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ProfileFunction> implements ProfileFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteCreateBefore__(ProfileFunction profileFunction, Properties properties,PersistenceFunctionCreator function) {
		super.__listenExecuteCreateBefore__(profileFunction, properties, function);
		//if user profile type then add corresponding keycloak role to the keycloak user
		if(ProfileType.CODE_UTILISATEUR.equals(profileFunction.getProfile().getType().getCode())) {
			Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByProfilesCodes(profileFunction.getProfile().getCode());
			if(CollectionHelper.isNotEmpty(actorProfiles)) {
				for(ActorProfile actorProfile : actorProfiles) {
					try {
						User user = UserManager.getInstance().readByUserName(actorProfile.getActor().getCode());
						if(user == null)
							continue;										
						UserManager.getInstance().addRolesByNames(List.of(user), profileFunction.getFunction().getCode());
					} catch (Exception exception) {
						LogHelper.logWarning(String.format("role %s not added in keycloak on user %s",profileFunction.getFunction().getCode(),actorProfile.getActor().getCode()), getClass());
					}
				}
			}
		}
	}
	
	@Override
	protected void __listenExecuteDeleteBefore__(ProfileFunction profileFunction, Properties properties,PersistenceFunctionRemover function) {
		super.__listenExecuteDeleteBefore__(profileFunction, properties, function);
		//if user profile type then delete corresponding keycloak role from the keycloak user
		if(ProfileType.CODE_UTILISATEUR.equals(profileFunction.getProfile().getType().getCode())) {
			Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByProfilesCodes(profileFunction.getProfile().getCode());
			if(CollectionHelper.isNotEmpty(actorProfiles)) {
				for(ActorProfile actorProfile : actorProfiles) {
					try {
						User user = UserManager.getInstance().readByUserName(actorProfile.getActor().getCode());
						if(user == null)
							continue;										
						UserManager.getInstance().deleteRolesByNames(List.of(user), profileFunction.getFunction().getCode());
					} catch (Exception exception) {
						LogHelper.logWarning(String.format("role %s not deleted in keycloak on user %s",profileFunction.getFunction().getCode(),actorProfile.getActor().getCode()), getClass());
					}
				}
			}
		}
	}
}