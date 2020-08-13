package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActivityEconomicNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityEconomicNature;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ActivityEconomicNaturePersistenceImpl extends AbstractPersistenceEntityImpl<ActivityEconomicNature> implements ActivityEconomicNaturePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}