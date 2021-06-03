package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.EntityManager;

import org.cyk.utility.__kernel__.collection.CollectionHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import org.cyk.utility.persistence.query.EntitySaver;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntitySaverImpl extends EntitySaver.AbstractImpl implements Serializable {

	@Override
	protected <T> void create(Class<T> tupleClass,Collection<T> collection, Listener<T> listener, EntityManager entityManager) {
		if(ProfileFunction.class.equals(tupleClass))
			__inject__(ProfileFunctionPersistence.class).createMany(CollectionHelper.cast(ProfileFunction.class, collection));
		else
			super.create(tupleClass, collection, listener, entityManager);
	}
	
	@Override
	protected <T> void delete(Class<T> tupleClass,Collection<T> collection, Listener<T> listener, EntityManager entityManager) {
		if(ProfileFunction.class.equals(tupleClass))
			__inject__(ProfileFunctionPersistence.class).deleteMany(CollectionHelper.cast(ProfileFunction.class, collection));
		else
			super.delete(tupleClass, collection, listener, entityManager);
	}
}