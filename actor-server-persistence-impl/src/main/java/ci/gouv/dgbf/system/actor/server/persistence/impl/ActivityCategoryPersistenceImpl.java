package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActivityCategoryPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActivityCategoryPersistenceImpl extends AbstractPersistenceEntityImpl<ActivityCategory> implements ActivityCategoryPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}