package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActivityCategoryBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActivityCategoryPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActivityCategoryBusinessImpl extends AbstractBusinessEntityImpl<ActivityCategory, ActivityCategoryPersistence> implements ActivityCategoryBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
