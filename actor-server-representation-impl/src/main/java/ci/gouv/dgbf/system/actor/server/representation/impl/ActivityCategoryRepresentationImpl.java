package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ActivityCategoryRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActivityCategoryDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ActivityCategoryRepresentationImpl extends AbstractRepresentationEntityImpl<ActivityCategoryDto> implements ActivityCategoryRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
