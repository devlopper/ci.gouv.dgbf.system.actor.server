package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.EntitySaver;

import ci.gouv.dgbf.system.actor.server.business.api.ActorProfileBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntitySaverImpl extends org.cyk.utility.business.server.EntitySaver.AbstractImpl implements Serializable {

	@SuppressWarnings("unchecked")
	@Override
	protected <T> void __save__(Class<T> tupleClass, Arguments<T> arguments) {
		if(ActorProfile.class.equals(tupleClass)) {
			EntitySaver.Arguments<ActorProfile> persistenceArguments = (org.cyk.utility.persistence.query.EntitySaver.Arguments<ActorProfile>) arguments.get__persistenceArguments__();
			if(CollectionHelper.isNotEmpty(persistenceArguments.getCreatables())) {
				__inject__(ActorProfileBusiness.class).createMany((Collection<ActorProfile>) persistenceArguments.getCreatables());
				persistenceArguments.setCreatables(null);
			}
			if(CollectionHelper.isNotEmpty(persistenceArguments.getDeletables())) {
				__inject__(ActorProfileBusiness.class).deleteMany((Collection<ActorProfile>) persistenceArguments.getDeletables());
				persistenceArguments.setDeletables(null);
			}
		}
		super.__save__(tupleClass, arguments);
	}	
}