package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.MeaEntity;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityLifeCycleListenerImpl extends EntityLifeCycleListener.AbstractImpl implements Serializable {

	@Override
	protected void listenBeforeCreate(Object object) {
		super.listenBeforeCreate(object);
		if(object instanceof MeaEntity)
			((MeaEntity)object).writeStatus();
	}
	
	@Override
	protected void listenBeforeUpdate(Object object) {
		super.listenBeforeUpdate(object);
		if(object instanceof Assignments)
			((MeaEntity)object).writeStatus();
	}
}