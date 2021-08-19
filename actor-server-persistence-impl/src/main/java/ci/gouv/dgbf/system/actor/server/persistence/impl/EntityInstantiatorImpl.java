package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.EntityInstantiator;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityInstantiatorImpl extends EntityInstantiator.AbstractImpl implements Serializable {

	@Override
	protected void __instantiate__(Object instance, Map<String, Object> fieldsNamesValues) {
		if(instance instanceof Profile)
			instantiateProfile((Profile) instance, fieldsNamesValues);
		else
			super.__instantiate__(instance, fieldsNamesValues);
	}
	
	/**/
	
	private void instantiateProfile(Profile profile,Map<String,Object> map) {
		if(profile.getType() == null && map.containsKey(Profile.FIELD_TYPE_IDENTIFIER)) {
			String identifier = (String) map.get(Profile.FIELD_TYPE_IDENTIFIER);
			profile.setType(EntityFinder.getInstance().find(ProfileType.class, identifier));
		}
	}
	
}