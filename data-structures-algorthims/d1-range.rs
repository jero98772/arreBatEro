#[derive(Debug, Clone)]
struct Job {
    id: usize,
    processing_time: u32,
}

impl Job {
    fn new(id: usize, processing_time: u32) -> Self {
        Job { id, processing_time }
    }
}

fn schedule_jobs(jobs: &[Job]) -> (Vec<Job>, Vec<Job>) {
    let mut machine1: Vec<Job> = Vec::new();
    let mut machine2: Vec<Job> = Vec::new();
    let mut time1: u32 = 0;
    let mut time2: u32 = 0;

    // Sort jobs by processing time in descending order (heuristic approach)
    let mut sorted_jobs = jobs.to_vec();
    sorted_jobs.sort_by(|a, b| b.processing_time.cmp(&a.processing_time));

    for job in sorted_jobs {
        if time1 <= time2 {
            machine1.push(job.clone());
            time1 += job.processing_time;
        } else {
            machine2.push(job.clone());
            time2 += job.processing_time;
        }
    }

    (machine1, machine2)
}

fn main() {
    // Example jobs with id and processing time
    let jobs = vec![
        Job::new(1, 10),
        Job::new(2, 5),
        Job::new(3, 8),
        Job::new(4, 7),
        Job::new(5, 3),
    ];

    let (machine1, machine2) = schedule_jobs(&jobs);

    println!("Machine 1 schedule: {:?}", machine1);
    println!("Machine 2 schedule: {:?}", machine2);
}

